use crate::prelude::*;
use crate::{AsFormat, TextRange};
use rome_formatter::cst_builders::{format_leading_comments, format_trailing_comments};
use rome_formatter::{
    format_args, write, Argument, Arguments, CstFormatContext, GroupId, VecBuffer,
};
use rome_js_syntax::{JsLanguage, JsSyntaxNode, JsSyntaxToken};
use rome_rowan::{AstNode, Direction, Language, SyntaxElement, SyntaxTriviaPiece};

/// Formats a node using its [`AsFormat`] implementation but falls back to printing the node as
/// it is in the source document if the formatting returns an [`FormatError`].
pub const fn format_or_verbatim<'a, Node>(node: &'a Node) -> FormatNodeOrVerbatim<'a, Node>
where
    Node: AstNode<Language = JsLanguage> + AsFormat<'a>,
{
    FormatNodeOrVerbatim { node }
}

/// Formats a node or falls back to verbatim printing if formating this node fails.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct FormatNodeOrVerbatim<'a, Node> {
    node: &'a Node,
}

impl<'a, Node> Format<JsFormatContext> for FormatNodeOrVerbatim<'a, Node>
where
    Node: AstNode<Language = JsLanguage> + AsFormat<'a>,
{
    fn fmt(&self, f: &mut JsFormatter) -> FormatResult<()> {
        let snapshot = Formatter::state_snapshot(f);

        match self.node.format().fmt(f) {
            Ok(result) => Ok(result),

            Err(_) => {
                f.restore_state_snapshot(snapshot);

                // Recover by formatting the nodes as they are written in the source document.
                // Doing so, the formatter formats the nodes/tokens as is.
                format_verbatim_node(self.node.syntax()).fmt(f)
            }
        }
    }
}

/// Adds parentheses around some content
/// Ensures that the leading trivia of the `first_content_token` is moved
/// before the opening parentheses and the trailing trivia of the `last_content_token`
/// is moved after the closing parentheses.
///
/// # Examples
/// Adding parentheses around the string literal
///
/// ```javascript
/// /* leading */ "test" /* trailing */;
/// ```
///
/// becomes
///
/// ```javascript
/// /* leading */ ("test") /* trailing */;
/// ```
pub fn format_parenthesize<Content>(content: &Content) -> FormatParenthesize
where
    Content: Format<JsFormatContext>,
{
    FormatParenthesize {
        content: Argument::new(content),
        grouped: false,
    }
}

/// Adds parentheses around an expression
#[derive(Clone)]
pub struct FormatParenthesize<'content> {
    grouped: bool,
    content: Argument<'content, JsFormatContext>,
}

impl FormatParenthesize<'_> {
    /// Groups the open parenthesis, the content, and the closing parenthesis inside of a group
    /// and indents the content with a soft block indent.
    pub fn grouped_with_soft_block_indent(mut self) -> Self {
        self.grouped = true;
        self
    }
}

impl Format<JsFormatContext> for FormatParenthesize<'_> {
    fn fmt(&self, f: &mut Formatter<JsFormatContext>) -> FormatResult<()> {
        if self.grouped {
            write!(
                f,
                [group_elements(&format_args![
                    token("("),
                    soft_block_indent(&Arguments::from(&self.content)),
                    token(")")
                ])]
            )
        } else {
            write!(f, [token("("), Arguments::from(&self.content), token(")")])
        }
    }
}

/// "Formats" a node according to its original formatting in the source text. Being able to format
/// a node "as is" is useful if a node contains syntax errors. Formatting a node with syntax errors
/// has the risk that Rome misinterprets the structure of the code and formatting it could
/// "mess up" the developers, yet incomplete, work or accidentally introduce new syntax errors.
///
/// You may be inclined to call `node.text` directly. However, using `text` doesn't track the nodes
/// nor its children source mapping information, resulting in incorrect source maps for this subtree.
///
/// These nodes and tokens get tracked as [FormatElement::Verbatim], useful to understand
/// if these nodes still need to have their own implementation.
pub fn format_verbatim_node(node: &JsSyntaxNode) -> FormatVerbatimNode {
    FormatVerbatimNode {
        node,
        kind: VerbatimKind::Verbatim {
            length: node.text_range().len(),
        },
        skip_comments: false,
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct FormatVerbatimNode<'node> {
    node: &'node JsSyntaxNode,
    kind: VerbatimKind,
    skip_comments: bool,
}

impl Format<JsFormatContext> for FormatVerbatimNode<'_> {
    fn fmt(&self, f: &mut JsFormatter) -> FormatResult<()> {
        for element in self.node.descendants_with_tokens(Direction::Next) {
            match element {
                SyntaxElement::Token(token) => f.state_mut().track_token(&token),
                SyntaxElement::Node(node) => {
                    f.context().comments().mark_suppression_checked(&node);
                }
            }
        }

        let mut buffer = VecBuffer::new(f.state_mut());

        write!(
            buffer,
            [format_with(|f| {
                if !self.skip_comments {
                    write!(f, [format_leading_comments(self.node)])?;
                }

                let node_trimmed_range = self.node.text_trimmed_range();

                let start = self
                    .node
                    .first_leading_trivia()
                    .into_iter()
                    .flat_map(|trivia| trivia.pieces())
                    .find(|piece| piece.is_skipped())
                    .map(|piece| piece.text_range().start())
                    .unwrap_or_else(|| node_trimmed_range.start());

                let node_range = self.node.text_range();

                let trimmed_range =
                    TextRange::at(start - node_range.start(), node_trimmed_range.end() - start);

                dynamic_token(
                    &normalize_newlines(
                        &self.node.text().slice(trimmed_range).to_string(),
                        LINE_TERMINATORS,
                    ),
                    start,
                )
                .fmt(f)?;

                if !self.skip_comments {
                    write!(f, [format_trailing_comments(self.node)])?;
                }

                Ok(())
            })]
        )?;

        let content = buffer.into_vec();

        let verbatim = Verbatim {
            content: content.into_boxed_slice(),
            kind: self.kind,
        };

        f.write_element(FormatElement::Verbatim(verbatim))
    }
}

/// Formats unknown nodes. The difference between this method  and `format_verbatim` is that this method
/// doesn't track nodes/tokens as [FormatElement::Verbatim]. They are just printed as they are.
pub fn format_unknown_node(node: &JsSyntaxNode) -> FormatUnknownNode {
    FormatUnknownNode { node }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FormatUnknownNode<'node> {
    node: &'node JsSyntaxNode,
}

impl Format<JsFormatContext> for FormatUnknownNode<'_> {
    fn fmt(&self, f: &mut JsFormatter) -> FormatResult<()> {
        FormatVerbatimNode {
            node: self.node,
            kind: VerbatimKind::Unknown,
            // Don't print comments because they get printed as part of the unknown node's `FormatNodeRule`
            skip_comments: true,
        }
        .fmt(f)
    }
}

/// Format a node having formatter suppression comment applied to it
pub fn format_suppressed_node(node: &JsSyntaxNode) -> FormatSuppressedNode {
    FormatSuppressedNode { node }
}

#[derive(Debug, Clone)]
pub struct FormatSuppressedNode<'node> {
    node: &'node JsSyntaxNode,
}

impl Format<JsFormatContext> for FormatSuppressedNode<'_> {
    fn fmt(&self, f: &mut JsFormatter) -> FormatResult<()> {
        // Insert a force a line break to ensure the suppression comment is on its own line
        // and correctly registers as a leading trivia on the opening token of this node
        write!(
            f,
            [
                hard_line_break(),
                FormatVerbatimNode {
                    node: self.node,
                    kind: VerbatimKind::Suppressed,
                    skip_comments: false
                }
            ]
        )
    }
}

/// Formats a group delimited by an opening and closing token,
/// such as a function body delimited by '{' and '}' tokens
///
/// Calling this method is required to correctly handle the comments attached
/// to the opening and closing tokens and insert them inside the group block
pub fn format_delimited<'a, 'content>(
    open_token: &'a JsSyntaxToken,
    content: &'content impl Format<JsFormatContext>,
    close_token: &'a JsSyntaxToken,
) -> FormatDelimited<'a, 'content> {
    FormatDelimited {
        open_token,
        content: Argument::new(content),
        close_token,
        mode: DelimitedMode::SoftBlockIndent(None),
    }
}

#[derive(Copy, Clone)]
pub struct FormatDelimited<'a, 'content> {
    open_token: &'a JsSyntaxToken,
    content: Argument<'content, JsFormatContext>,
    close_token: &'a JsSyntaxToken,
    mode: DelimitedMode,
}

impl FormatDelimited<'_, '_> {
    fn with_mode(mut self, mode: DelimitedMode) -> Self {
        self.mode = mode;
        self
    }

    /// Formats a group delimited by an opening and closing token, placing the
    /// content in a [block_indent] group
    pub fn block_indent(self) -> Self {
        self.with_mode(DelimitedMode::BlockIndent)
    }

    /// Formats a group delimited by an opening and closing token, placing the
    /// content in a [soft_block_indent] group
    pub fn soft_block_indent(self) -> Self {
        self.with_mode(DelimitedMode::SoftBlockIndent(None))
    }

    /// Formats a group delimited by an opening and closing token, placing the
    /// content in an [indent] group with [soft_line_break_or_space] tokens at the
    /// start and end
    pub fn soft_block_spaces(self) -> Self {
        self.with_mode(DelimitedMode::SoftBlockSpaces(None))
    }

    pub fn soft_block_indent_with_group_id(self, group_id: Option<GroupId>) -> Self {
        self.with_mode(DelimitedMode::SoftBlockIndent(group_id))
    }
}

impl Format<JsFormatContext> for FormatDelimited<'_, '_> {
    fn fmt(&self, f: &mut JsFormatter) -> FormatResult<()> {
        let FormatDelimited {
            open_token,
            close_token,
            content,
            mode,
        } = self;

        let delimited = format_with(|f| {
            open_token.format().fmt(f)?;

            let format_content = format_with(|f| f.write_fmt(Arguments::from(content)));

            match mode {
                DelimitedMode::BlockIndent => block_indent(&format_content).fmt(f)?,
                DelimitedMode::SoftBlockIndent(_) => soft_block_indent(&format_content).fmt(f)?,
                DelimitedMode::SoftBlockSpaces(_) => {
                    let mut is_empty = true;

                    let format_content = format_once(|f| {
                        let mut buffer = f.inspect(|element| {
                            if !element.is_empty() {
                                is_empty = false
                            }
                        });

                        write!(buffer, [format_content])
                    });

                    soft_line_indent_or_space(&format_content).fmt(f)?;

                    if !is_empty {
                        soft_line_break_or_space().fmt(f)?;
                    }
                }
            };

            close_token.format().fmt(f)
        });

        match mode {
            // Group is useless, the block indent would expand it right anyway
            DelimitedMode::BlockIndent => write!(f, [delimited])?,
            DelimitedMode::SoftBlockIndent(group_id) | DelimitedMode::SoftBlockSpaces(group_id) => {
                match group_id {
                    None => write!(f, [group_elements(&delimited)])?,
                    Some(group_id) => write!(
                        f,
                        [group_elements(&delimited).with_group_id(Some(*group_id))]
                    )?,
                }
            }
        };

        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum DelimitedMode {
    BlockIndent,
    SoftBlockIndent(Option<GroupId>),
    SoftBlockSpaces(Option<GroupId>),
}
