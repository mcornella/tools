use crate::prelude::*;
use rome_formatter::cst_builders::format_dangling_trivia;
use rome_formatter::{write, Buffer, CstFormatContext};
use rome_js_syntax::JsAnyStatement;
use rome_js_syntax::JsBlockStatement;

use rome_js_syntax::JsBlockStatementFields;
use rome_js_syntax::JsSyntaxKind;
use rome_rowan::{AstNode, AstNodeList};

#[derive(Debug, Clone, Default)]
pub struct FormatJsBlockStatement;

impl FormatNodeRule<JsBlockStatement> for FormatJsBlockStatement {
    fn fmt_fields(&self, node: &JsBlockStatement, f: &mut JsFormatter) -> FormatResult<()> {
        let JsBlockStatementFields {
            l_curly_token,
            statements,
            r_curly_token,
        } = node.as_fields();

        let r_curly_token = r_curly_token?;

        write!(f, [l_curly_token.format()])?;

        if statements.is_empty() {
            if f.context().comments().has_dangling_trivia(&r_curly_token) {
                write!(f, [format_dangling_trivia(&r_curly_token).indented()])?;
            } else if !can_collapse_empty_block(node) {
                write!(f, [hard_line_break()])?;
            }
        } else {
            write!(
                f,
                [block_indent(&format_with(|f| {
                    let all_empty = statements
                        .iter()
                        .all(|stmt| matches!(stmt, JsAnyStatement::JsEmptyStatement(_)));

                    // The formatting of empty statements removes the statements. Let's make sure that a block
                    // that only contained empty statements won't collapse if it isn't allowed to.
                    if all_empty && !can_collapse_empty_block(node) {
                        write!(f, [hard_line_break()])?;
                    }

                    write!(f, [statements.format()])
                }))]
            )?;
        }

        write!(f, [r_curly_token.format()])
    }
}

fn can_collapse_empty_block(block: &JsBlockStatement) -> bool {
    // reference https://github.com/prettier/prettier/blob/b188c905cfaeb238a122b4a95c230da83f2f3226/src/language-js/print/block.js#L19
    match block.syntax().parent() {
        Some(parent) => match parent.kind() {
            JsSyntaxKind::JS_FOR_STATEMENT
            | JsSyntaxKind::JS_WHILE_STATEMENT
            | JsSyntaxKind::JS_DO_WHILE_STATEMENT
            | JsSyntaxKind::TS_MODULE_DECLARATION
            | JsSyntaxKind::TS_DECLARE_FUNCTION_DECLARATION => true,

            // prettier collapse the catch block when it doesn't have a `finalizer`, insert a new line when it has a `finalizer`
            JsSyntaxKind::JS_CATCH_CLAUSE => {
                let finally_clause = parent.next_sibling();
                !matches!(
                    finally_clause.map(|finally| finally.kind()),
                    Some(JsSyntaxKind::JS_FINALLY_CLAUSE),
                )
            }
            _ => false,
        },
        None => false,
    }
}
