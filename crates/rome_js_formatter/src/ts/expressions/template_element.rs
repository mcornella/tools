use crate::prelude::*;
use crate::utils::TemplateElement;
use rome_formatter::write;

use rome_js_syntax::TsTemplateElement;

#[derive(Debug, Clone, Default)]
pub struct FormatTsTemplateElement;

impl FormatNodeRule<TsTemplateElement> for FormatTsTemplateElement {
    fn fmt_fields(&self, node: &TsTemplateElement, f: &mut JsFormatter) -> FormatResult<()> {
        write!(f, [TemplateElement::Ts(node.clone())])
    }
}
