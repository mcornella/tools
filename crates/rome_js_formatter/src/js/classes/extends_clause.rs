use crate::prelude::*;

use rome_formatter::{format_args, write, CstFormatContext};
use rome_js_syntax::JsExtendsClause;
use rome_js_syntax::JsExtendsClauseFields;
use rome_js_syntax::JsSyntaxKind::JS_ASSIGNMENT_EXPRESSION;

#[derive(Debug, Clone, Default)]
pub struct FormatJsExtendsClause;

impl FormatNodeRule<JsExtendsClause> for FormatJsExtendsClause {
    fn fmt_fields(&self, node: &JsExtendsClause, f: &mut JsFormatter) -> FormatResult<()> {
        let JsExtendsClauseFields {
            extends_token,
            super_class,
            type_arguments,
        } = node.as_fields();

        let super_class = super_class?;

        let format_super = format_with(|f: &mut JsFormatter| {
            let content =
                format_with(|f| write!(f, [super_class.format(), type_arguments.format()]));
            let comments = f.context().comments();

            let has_trailing_comments = if let Some(type_arguments) = &type_arguments {
                comments.has_trailing_comments(type_arguments.syntax())
            } else {
                comments.has_trailing_comments(super_class.syntax())
            };

            if node
                .syntax()
                .parent()
                .map_or(false, |p| p.kind() == JS_ASSIGNMENT_EXPRESSION)
            {
                if comments.has_leading_comments(super_class.syntax()) || has_trailing_comments {
                    write!(f, [format_parenthesize(&content,)])
                } else {
                    let content = content.memoized();
                    write!(
                        f,
                        [
                            if_group_breaks(&format_args![
                                // Format_inserted is fine here because it is known that super has no comments
                                token("("),
                                soft_block_indent(&content),
                                token(")"),
                            ]),
                            if_group_fits_on_line(&content)
                        ]
                    )
                }
            } else {
                content.fmt(f)
            }
        });

        write![
            f,
            [
                extends_token.format(),
                space_token(),
                group_elements(&format_super)
            ]
        ]
    }
}
