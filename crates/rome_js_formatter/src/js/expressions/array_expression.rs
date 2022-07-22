use crate::prelude::*;
use rome_formatter::cst_builders::format_dangling_trivia;

use rome_formatter::write;
use rome_js_syntax::JsArrayExpression;
use rome_js_syntax::JsArrayExpressionFields;

#[derive(Debug, Clone, Default)]
pub struct FormatJsArrayExpression;

impl FormatNodeRule<JsArrayExpression> for FormatJsArrayExpression {
    fn fmt_fields(&self, node: &JsArrayExpression, f: &mut JsFormatter) -> FormatResult<()> {
        let JsArrayExpressionFields {
            l_brack_token,
            elements,
            r_brack_token,
        } = node.as_fields();

        let r_brack_token = r_brack_token?;

        if elements.is_empty() {
            write!(
                f,
                [
                    l_brack_token.format(),
                    format_dangling_trivia(&r_brack_token).indented(),
                    r_brack_token.format()
                ]
            )
        } else {
            let group_id = f.group_id("array");

            let elements = elements.format().with_options(Some(group_id));

            write!(
                f,
                [format_delimited(&l_brack_token?, &elements, &r_brack_token)
                    .soft_block_indent_with_group_id(Some(group_id))]
            )
        }
    }
}
