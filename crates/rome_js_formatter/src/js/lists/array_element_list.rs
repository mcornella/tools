use crate::prelude::*;
use rome_formatter::{Comments, CstFormatContext, FormatRuleWithOptions, GroupId};
use std::convert::Infallible;

use crate::utils::array::write_array_node;

use rome_js_syntax::{JsAnyExpression, JsArrayElementList, JsLanguage, JsSyntaxNode};
use rome_rowan::{AstNode, AstSeparatedList, Direction};

#[derive(Debug, Clone, Default)]
pub struct FormatJsArrayElementList {
    group_id: Option<GroupId>,
}

impl FormatRuleWithOptions<JsArrayElementList> for FormatJsArrayElementList {
    type Options = Option<GroupId>;

    fn with_options(mut self, options: Self::Options) -> Self {
        self.group_id = options;
        self
    }
}

impl FormatRule<JsArrayElementList> for FormatJsArrayElementList {
    type Context = JsFormatContext;

    fn fmt(&self, node: &JsArrayElementList, f: &mut JsFormatter) -> FormatResult<()> {
        let comments = f.context().comments();
        if !has_formatter_trivia(node.syntax(), &comments) && can_print_fill(node, &comments) {
            // Using format_separated is valid in this case as can_print_fill does not allow holes
            return f
                .fill(soft_line_break_or_space())
                .entries(node.format_separated(",").with_group_id(self.group_id))
                .finish();
        }

        write_array_node(node, f)
    }
}

/// Returns true if the provided JsArrayElementList could
/// be "fill-printed" instead of breaking each element on
/// a different line.
///
/// The underlying logic only allows lists of literal expressions
/// with 10 or less characters, potentially wrapped in a "short"
/// unary expression (+, -, ~ or !)
fn can_print_fill(list: &JsArrayElementList, comments: &Comments<JsLanguage>) -> bool {
    use rome_js_syntax::JsAnyArrayElement::*;
    use rome_js_syntax::JsAnyExpression::*;
    use rome_js_syntax::JsUnaryOperator::*;

    list.iter().all(|item| {
        if let Ok(element) = item {
            if comments.has_comments(element.syntax()) {
                return false;
            }

            match element {
                JsAnyExpression(JsUnaryExpression(expr)) => {
                    match expr.operator() {
                        Ok(Plus | Minus | BitwiseNot | LogicalNot) => {}
                        _ => return false,
                    }

                    if let Ok(expr) = expr.argument() {
                        is_short_literal(&expr, comments)
                    } else {
                        false
                    }
                }
                JsAnyExpression(expr) => is_short_literal(&expr, comments),
                _ => false,
            }
        } else {
            false
        }
    })
}

/// Returns true if the provided expression is a literal with 10 or less characters
fn is_short_literal(expr: &JsAnyExpression, comments: &Comments<JsLanguage>) -> bool {
    match expr {
        JsAnyExpression::JsAnyLiteralExpression(lit) => {
            if comments.has_comments(lit.syntax()) {
                return false;
            }

            let token_len = lit
                .syntax()
                .text_trimmed()
                .try_fold_chunks::<_, _, Infallible>(0, |sum, chunk| {
                    // Count actual characters instead of byte length
                    Ok(sum + chunk.chars().count())
                })
                .expect("the above fold operation is infallible");

            token_len <= 10
        }
        _ => false,
    }
}

/// Returns true if this node contains "printable" trivias: comments
/// or empty lines (2 consecutive newlines only separated by whitespace)
fn has_formatter_trivia(node: &JsSyntaxNode, comments: &Comments<JsLanguage>) -> bool {
    if comments.has_comments(node) {
        return true;
    }

    for token in node.descendants_tokens(Direction::Next) {
        let mut line_count = 0;

        if comments.has_dangling_trivia(&token) {
            return true;
        }

        for trivia in token.leading_trivia().pieces() {
            if trivia.is_newline() {
                line_count += 1;
                if line_count >= 2 {
                    return true;
                }
            }
        }

        // trailing has never new lines, safe to skip.
    }

    false
}
