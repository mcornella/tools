pub(crate) mod array;
mod assignment_like;
mod binary_like_expression;
mod format_conditional;
mod simple;
pub mod string_utils;

pub(crate) mod format_class;
pub mod jsx_utils;
mod member_chain;
mod object;
mod object_like;
mod object_pattern_like;
#[cfg(test)]
mod quickcheck_utils;
mod typescript;

use crate::prelude::*;
pub(crate) use assignment_like::{should_break_after_operator, JsAnyAssignmentLike};
pub(crate) use binary_like_expression::{format_binary_like_expression, JsAnyBinaryLikeExpression};
pub(crate) use format_conditional::{format_conditional, Conditional};
pub(crate) use member_chain::format_call_expression;
pub(crate) use object_like::JsObjectLike;
pub(crate) use object_pattern_like::JsObjectPatternLike;
use rome_formatter::{
    format_args, normalize_newlines, write, Buffer, Comments, CstFormatContext, VecBuffer,
};
use rome_js_syntax::{
    JsAnyExpression, JsAnyFunction, JsAnyStatement, JsCallArgumentList, JsComputedMemberExpression,
    JsInitializerClause, JsLanguage, JsStaticMemberExpression, JsTemplateElement, Modifiers,
    TsTemplateElement, TsType,
};
use rome_js_syntax::{JsSyntaxKind, JsSyntaxNode, JsSyntaxToken};
use rome_rowan::{declare_node_union, AstNode, AstNodeList, SyntaxNode, SyntaxResult};

pub(crate) use typescript::should_hug_type;

pub(crate) use simple::*;
pub(crate) use string_utils::*;

/// Utility function to format the separators of the nodes that belong to the unions
/// of [rome_js_syntax::TsAnyTypeMember].
///
/// We can have two kind of separators: `,`, `;` or ASI.
/// Because of how the grammar crafts the nodes, the parent will add the separator to the node.
/// So here, we create - on purpose - an empty node.
pub struct FormatTypeMemberSeparator<'a> {
    token: Option<&'a JsSyntaxToken>,
}

impl<'a> FormatTypeMemberSeparator<'a> {
    pub fn new(token: Option<&'a JsSyntaxToken>) -> Self {
        Self { token }
    }
}

impl Format<JsFormatContext> for FormatTypeMemberSeparator<'_> {
    fn fmt(&self, f: &mut JsFormatter) -> FormatResult<()> {
        if let Some(separator) = self.token {
            format_removed(separator).fmt(f)
        } else {
            Ok(())
        }
    }
}

/// Utility function to format the node [rome_js_syntax::JsInitializerClause]
pub struct FormatInitializerClause<'a> {
    initializer: Option<&'a JsInitializerClause>,
}

impl<'a> FormatInitializerClause<'a> {
    pub fn new(initializer: Option<&'a JsInitializerClause>) -> Self {
        Self { initializer }
    }
}

impl Format<JsFormatContext> for FormatInitializerClause<'_> {
    fn fmt(&self, f: &mut JsFormatter) -> FormatResult<()> {
        if let Some(initializer) = self.initializer {
            write!(f, [space_token(), initializer.format()])
        } else {
            Ok(())
        }
    }
}

pub struct FormatInterpreterToken<'a> {
    token: Option<&'a JsSyntaxToken>,
}

impl<'a> FormatInterpreterToken<'a> {
    pub fn new(interpreter_token: Option<&'a JsSyntaxToken>) -> Self {
        Self {
            token: interpreter_token,
        }
    }
}

impl Format<JsFormatContext> for FormatInterpreterToken<'_> {
    fn fmt(&self, f: &mut JsFormatter) -> FormatResult<()> {
        if let Some(interpreter) = self.token {
            write!(f, [interpreter.format(), empty_line()])
        } else {
            Ok(())
        }
    }
}

/// Returns true if this node contains newlines in trivias.
pub(crate) fn node_has_leading_newline(node: &JsSyntaxNode) -> bool {
    if let Some(leading_trivia) = node.first_leading_trivia() {
        for piece in leading_trivia.pieces() {
            if piece.is_newline() {
                return true;
            }
        }
    }
    false
}

/// Format an element with a single line head and a body that might
/// be either a block or a single statement.
pub struct FormatBodyStatement<'a> {
    body: &'a JsAnyStatement,
}

impl<'a> FormatBodyStatement<'a> {
    pub fn new(statement: &'a JsAnyStatement) -> Self {
        Self { body: statement }
    }
}

impl Format<JsFormatContext> for FormatBodyStatement<'_> {
    fn fmt(&self, f: &mut JsFormatter) -> FormatResult<()> {
        match self.body {
            JsAnyStatement::JsEmptyStatement(body) => {
                write!(f, [body.format(), token(";")])
            }
            body => {
                write!(f, [space_token(), body.format()])
            }
        }
    }
}

/// This function consumes a list of modifiers and applies a predictable sorting.
pub(crate) fn sort_modifiers_by_precedence<List, Node>(list: &List) -> Vec<Node>
where
    Node: AstNode<Language = JsLanguage>,
    List: AstNodeList<Language = JsLanguage, Node = Node>,
    Modifiers: for<'a> From<&'a Node>,
{
    let mut nodes_and_modifiers = list.iter().collect::<Vec<Node>>();

    nodes_and_modifiers.sort_unstable_by_key(|node| Modifiers::from(node));

    nodes_and_modifiers
}

/// Utility to format
pub(crate) fn format_template_chunk(chunk: JsSyntaxToken, f: &mut JsFormatter) -> FormatResult<()> {
    // Per https://tc39.es/ecma262/multipage/ecmascript-language-lexical-grammar.html#sec-static-semantics-trv:
    // In template literals, the '\r' and '\r\n' line terminators are normalized to '\n'

    write!(
        f,
        [format_replaced(
            &chunk,
            &syntax_token_cow_slice(
                normalize_newlines(chunk.text_trimmed(), ['\r']),
                &chunk,
                chunk.text_trimmed_range().start(),
            )
        )]
    )
}

pub(crate) enum TemplateElement {
    Js(JsTemplateElement),
    Ts(TsTemplateElement),
}

impl Format<JsFormatContext> for TemplateElement {
    fn fmt(&self, f: &mut JsFormatter) -> FormatResult<()> {
        let comments = f.context().comments();
        let is_simple = self.is_plain_expression(&comments)?;

        write!(
            f,
            [group_elements(&format_args![
                self.dollar_curly_token().format(),
                format_with(|f: &mut JsFormatter| {
                    let format_expression = format_with(|f| match self {
                        TemplateElement::Js(template) => {
                            write!(f, [template.expression().format()])
                        }
                        TemplateElement::Ts(template) => {
                            write!(f, [template.ty().format()])
                        }
                    });

                    if !is_simple {
                        let expression = self.expression()?;

                        if comments.has_comments(&expression)
                            || matches!(
                                expression.kind(),
                                JsSyntaxKind::JS_STATIC_MEMBER_EXPRESSION
                                    | JsSyntaxKind::JS_COMPUTED_MEMBER_EXPRESSION
                                    | JsSyntaxKind::JS_CONDITIONAL_EXPRESSION
                                    | JsSyntaxKind::JS_SEQUENCE_EXPRESSION
                                    | JsSyntaxKind::TS_CONDITIONAL_TYPE
                                    | JsSyntaxKind::JS_PARENTHESIZED_EXPRESSION
                                    | JsSyntaxKind::TS_AS_EXPRESSION
                            )
                            || JsAnyBinaryLikeExpression::can_cast(expression.kind())
                        {
                            write!(f, [soft_block_indent(&format_expression)])
                        } else {
                            write!(f, [format_expression])
                        }
                    } else {
                        write!(f, [format_expression])
                    }
                }),
                line_suffix_boundary(),
                self.r_curly_token().format()
            ])]
        )
    }
}

impl TemplateElement {
    fn dollar_curly_token(&self) -> SyntaxResult<JsSyntaxToken> {
        match self {
            TemplateElement::Js(template) => template.dollar_curly_token(),
            TemplateElement::Ts(template) => template.dollar_curly_token(),
        }
    }

    fn r_curly_token(&self) -> SyntaxResult<JsSyntaxToken> {
        match self {
            TemplateElement::Js(template) => template.r_curly_token(),
            TemplateElement::Ts(template) => template.r_curly_token(),
        }
    }

    /// We want to break the template element only when we have articulated expressions inside it.
    ///
    /// We a plain expression is when it's one of the following:
    /// - `loreum ${this.something} ipsum`
    /// - `loreum ${a.b.c} ipsum`
    /// - `loreum ${a} ipsum`
    fn is_plain_expression(&self, comments: &Comments<JsLanguage>) -> FormatResult<bool> {
        match self {
            TemplateElement::Js(template_element) => {
                let mut current_expression = template_element.expression()?;

                if comments.has_comments(current_expression.syntax()) {
                    return Ok(false);
                }

                match current_expression {
                    JsAnyExpression::JsStaticMemberExpression(expression) => {
                        JsAnyMemberExpression::from(expression)
                            .is_simple_template_expression(comments)
                    }
                    JsAnyExpression::JsComputedMemberExpression(expression) => {
                        JsAnyMemberExpression::from(expression)
                            .is_simple_template_expression(comments)
                    }
                    JsAnyExpression::JsIdentifierExpression(_)
                    | JsAnyExpression::JsAnyLiteralExpression(_)
                    | JsAnyExpression::JsCallExpression(_) => Ok(true),

                    JsAnyExpression::JsParenthesizedExpression(expression) => {
                        // binary and logical expression have their own grouping inside parenthesis,
                        // so we mark the current parenthesized expression as not plain
                        match expression.expression()? {
                            JsAnyExpression::JsLogicalExpression(_)
                            | JsAnyExpression::JsBinaryExpression(_) => Ok(false),
                            _ => Ok(true),
                        }
                    }

                    _ => {
                        if let Some(function) =
                            JsAnyFunction::cast(current_expression.syntax().clone())
                        {
                            Ok(is_simple_function_expression(function, comments)?)
                        } else {
                            Ok(false)
                        }
                    }
                }
            }
            TemplateElement::Ts(template_element) => {
                if comments.has_comments(template_element.syntax()) {
                    return Ok(false);
                }

                let is_mapped_type = matches!(template_element.ty()?, TsType::TsMappedType(_));
                Ok(!is_mapped_type)
            }
        }
    }

    fn expression(&self) -> SyntaxResult<SyntaxNode<JsLanguage>> {
        match self {
            TemplateElement::Js(template) => template.expression().map(|node| node.into_syntax()),
            TemplateElement::Ts(template) => template.ty().map(|node| node.into_syntax()),
        }
    }
}

declare_node_union! {
    JsAnyMemberExpression = JsStaticMemberExpression
        | JsComputedMemberExpression
}

impl JsAnyMemberExpression {
    fn object_syntax(&self) -> SyntaxResult<SyntaxNode<JsLanguage>> {
        match self {
            JsAnyMemberExpression::JsStaticMemberExpression(static_member) => {
                static_member.object().map(|node| node.into_syntax())
            }
            JsAnyMemberExpression::JsComputedMemberExpression(computed) => {
                computed.object().map(|node| node.into_syntax())
            }
        }
    }

    fn is_simple_template_expression(&self, comments: &Comments<JsLanguage>) -> FormatResult<bool> {
        if comments.has_comments(self.syntax()) {
            return Ok(false);
        }

        let mut object = self.syntax().clone();

        while JsAnyMemberExpression::can_cast(object.kind()) {
            let member_expression = JsAnyMemberExpression::unwrap_cast(object);

            object = member_expression.object_syntax()?;

            if comments.has_trailing_comments(&object) {
                return Ok(false);
            }
        }

        Ok(matches!(
            object.kind(),
            JsSyntaxKind::JS_IDENTIFIER_EXPRESSION | JsSyntaxKind::JS_THIS_EXPRESSION
        ))
    }
}

/// This enum is used to extract a precedence from certain nodes. By comparing the precedence
/// of two nodes, it's possible to change the way certain node should be formatted.
///
/// A use case, for example, is when comparing a node with its parent. If the parent has a lower
/// precedence, then the node can change its formatting.
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) enum FormatPrecedence {
    /// No precedence given to these nodes
    None,

    /// Low priority
    Low,

    /// High priority
    High,
}

impl FormatPrecedence {
    /// Use this function when you want to extract the precedence of the current node
    /// based on whether it can parenthesised or not.
    ///
    /// This function is useful when we want to compare a node against its parent. If the parent has
    /// lower precedence, it means that we can remove the parenthesis from the current node.
    ///
    /// An example can be:
    ///
    /// ```js
    /// let a = ("simple expression") + " or not";
    /// ```
    ///
    /// In this case, we have a parenthesised expression and its parent is a binary expression.
    /// The first one will have [FormatPrecedence::Low] as priority and the second has
    /// [FormatPrecedence::None] as priority. In this case, the parenthesis can be omitted.
    pub fn with_precedence_for_parenthesis(node: Option<&JsSyntaxNode>) -> Self {
        node.map_or(FormatPrecedence::None, |node| match node.kind() {
            JsSyntaxKind::JS_PARENTHESIZED_EXPRESSION => FormatPrecedence::Low,

            JsSyntaxKind::TS_AS_EXPRESSION
            | JsSyntaxKind::TS_NON_NULL_ASSERTION_EXPRESSION
            | JsSyntaxKind::TS_TYPE_ASSERTION_EXPRESSION
            | JsSyntaxKind::JS_UNARY_EXPRESSION
            | JsSyntaxKind::JS_LOGICAL_EXPRESSION
            | JsSyntaxKind::JS_BINARY_EXPRESSION
            | JsSyntaxKind::JS_TEMPLATE
            | JsSyntaxKind::JS_SPREAD
            | JsSyntaxKind::JS_STATIC_MEMBER_EXPRESSION
            | JsSyntaxKind::JS_STATIC_MEMBER_ASSIGNMENT
            | JsSyntaxKind::JS_CALL_EXPRESSION
            | JsSyntaxKind::JS_NEW_EXPRESSION
            | JsSyntaxKind::JS_CONDITIONAL_EXPRESSION
            | JsSyntaxKind::JS_EXTENDS_CLAUSE
            | JsSyntaxKind::TS_IMPLEMENTS_CLAUSE
            | JsSyntaxKind::JS_AWAIT_EXPRESSION
            | JsSyntaxKind::JS_YIELD_ARGUMENT
            | JsSyntaxKind::JS_ARROW_FUNCTION_EXPRESSION
            | JsSyntaxKind::JS_EXPRESSION_STATEMENT
            | JsSyntaxKind::JS_RETURN_STATEMENT
            | JsSyntaxKind::JS_COMPUTED_MEMBER_EXPRESSION
            | JsSyntaxKind::JS_COMPUTED_MEMBER_ASSIGNMENT => FormatPrecedence::High,

            _ => FormatPrecedence::None,
        })
    }
}

/// Format a some code followed by an optional semicolon, and performs
/// semicolon insertion if it was missing in the input source and the
/// preceding element wasn't an unknown node
pub struct FormatWithSemicolon<'a> {
    content: &'a dyn Format<JsFormatContext>,
    semicolon: Option<&'a JsSyntaxToken>,
}

impl<'a> FormatWithSemicolon<'a> {
    pub fn new(
        content: &'a dyn Format<JsFormatContext>,
        semicolon: Option<&'a JsSyntaxToken>,
    ) -> Self {
        Self { content, semicolon }
    }
}

impl Format<JsFormatContext> for FormatWithSemicolon<'_> {
    fn fmt(&self, f: &mut JsFormatter) -> FormatResult<()> {
        let mut buffer = VecBuffer::new(f.state_mut());

        write!(buffer, [self.content])?;

        let content = buffer.into_element();

        let is_unknown = match content.last_element() {
            Some(FormatElement::Verbatim(elem)) => elem.is_unknown(),
            _ => false,
        };

        f.write_element(content)?;

        if let Some(semicolon) = self.semicolon {
            write!(f, [semicolon.format()])?;
        } else if !is_unknown {
            token(";").fmt(f)?;
        }
        Ok(())
    }
}

/// A call like expression is one of:
///
/// - [JsNewExpression]
/// - [JsImportCallExpression]
/// - [JsCallExpression]
pub(crate) fn is_call_like_expression(expression: &JsAnyExpression) -> bool {
    matches!(
        expression,
        JsAnyExpression::JsNewExpression(_)
            | JsAnyExpression::JsImportCallExpression(_)
            | JsAnyExpression::JsCallExpression(_)
    )
}

/// This function is in charge to format the call arguments.
pub(crate) fn write_arguments_multi_line<S: Format<JsFormatContext>, I>(
    arguments: &JsCallArgumentList,
    separated: I,
    f: &mut JsFormatter,
) -> FormatResult<()>
where
    I: Iterator<Item = S>,
{
    let with_node = arguments.iter().zip(separated);
    let mut iterator = with_node.peekable();
    let mut join_with = f.join_nodes_with_soft_line();

    while let Some((node, element)) = iterator.next() {
        let last = iterator.peek().is_none();

        if last {
            join_with.entry(
                node?.syntax(),
                &format_args![&element, &if_group_breaks(&token(","))],
            );
        } else {
            join_with.entry(node?.syntax(), &element);
        }
    }

    join_with.finish()
}
