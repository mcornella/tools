use crate::prelude::*;
use crate::utils::TemplateElement;
use rome_formatter::write;

use rome_js_syntax::JsTemplateElement;

#[derive(Debug, Clone, Default)]
pub struct FormatJsTemplateElement;

impl FormatNodeRule<JsTemplateElement> for FormatJsTemplateElement {
    fn fmt_fields(&self, node: &JsTemplateElement, f: &mut JsFormatter) -> FormatResult<()> {
        write!(f, [TemplateElement::Js(node.clone())])
    }
}
