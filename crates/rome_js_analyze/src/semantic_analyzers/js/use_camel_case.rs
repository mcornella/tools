use crate::{
    semantic_services::Semantic,
    utils::{rename::RenameSymbolExtensions, ToCamelCase},
    JsRuleAction,
};
use rome_analyze::{
    context::RuleContext, declare_rule, ActionCategory, Rule, RuleCategory, RuleDiagnostic,
};
use rome_console::markup;
use rome_diagnostics::Applicability;
use rome_js_syntax::{
    JsFormalParameter, JsFunctionDeclaration, JsFunctionExportDefaultDeclaration,
    JsGetterClassMember, JsIdentifierBinding, JsLiteralMemberName, JsMethodClassMember,
    JsPrivateClassMemberName, JsPropertyClassMember, JsSetterClassMember, JsSyntaxKind,
    JsVariableDeclarator,
};
use rome_rowan::{declare_node_union, AstNode, BatchMutationExt};
use std::{borrow::Cow, iter::once};

declare_rule! {
    /// Enforce camel case naming convention.
    ///
    /// ## Examples
    ///
    /// ### Invalid
    ///
    /// ```js,expect_diagnostic
    /// let snake_case;
    /// ```
    ///
    /// ```js,expect_diagnostic
    /// let PascalCase;
    /// ```
    ///
    /// ## Valid
    ///
    /// ```js
    /// let camelCase;
    /// ```
    pub(crate) UseCamelCase {
        version: "0.8.0",
        name: "useCamelCase",
        recommended: false,
    }
}

pub struct State {
    new_name: String,
}

fn check_is_camel(name: &str) -> Option<State> {
    if name.starts_with('_') {
        return None;
    }

    match name.to_camel_case() {
        Cow::Borrowed(_) => None,
        Cow::Owned(new_name) => Some(State { new_name }),
    }
}

impl Rule for UseCamelCase {
    const CATEGORY: RuleCategory = RuleCategory::Lint;

    type Query = Semantic<JsAnyCamelCaseName>;
    type State = State;
    type Signals = Option<Self::State>;

    fn run(ctx: &RuleContext<Self>) -> Option<Self::State> {
        let name = ctx.query();

        match name {
            JsAnyCamelCaseName::JsIdentifierBinding(binding) => {
                let is_variable = binding.parent::<JsVariableDeclarator>().is_some();
                let is_parameter = binding.parent::<JsFormalParameter>().is_some();
                let is_function = binding.parent::<JsFunctionDeclaration>().is_some();
                let is_exported_function = binding
                    .parent::<JsFunctionExportDefaultDeclaration>()
                    .is_some();
                if is_variable || is_parameter || is_function || is_exported_function {
                    let name = binding.name_token().ok()?;
                    check_is_camel(name.text_trimmed())
                } else {
                    None
                }
            }
            JsAnyCamelCaseName::JsLiteralMemberName(name) => {
                let is_method_class = name.parent::<JsMethodClassMember>().is_some();
                let is_getter = name.parent::<JsGetterClassMember>().is_some();
                let is_setter = name.parent::<JsSetterClassMember>().is_some();
                let is_property = name.parent::<JsPropertyClassMember>().is_some();
                if is_method_class || is_getter || is_setter || is_property {
                    let name = name.text();
                    check_is_camel(&name)
                } else {
                    None
                }
            }
            JsAnyCamelCaseName::JsPrivateClassMemberName(name) => {
                let is_property = name.parent::<JsPropertyClassMember>().is_some();
                if is_property {
                    let name = name.text();
                    check_is_camel(&name)
                } else {
                    None
                }
            }
        }
    }

    fn diagnostic(ctx: &RuleContext<Self>, _: &Self::State) -> Option<RuleDiagnostic> {
        let binding = ctx.query();

        let symbol_type = match binding.syntax().parent().unwrap().kind() {
            JsSyntaxKind::JS_FORMAL_PARAMETER => "parameters",
            JsSyntaxKind::JS_FUNCTION_DECLARATION => "functions",
            JsSyntaxKind::JS_GETTER_CLASS_MEMBER
            | JsSyntaxKind::JS_SETTER_CLASS_MEMBER
            | JsSyntaxKind::JS_METHOD_CLASS_MEMBER => "methods",
            JsSyntaxKind::JS_PROPERTY_CLASS_MEMBER | JsSyntaxKind::JS_PRIVATE_CLASS_MEMBER_NAME => {
                "properties"
            }
            _ => "variables",
        };

        let diag = RuleDiagnostic::new(
            binding.syntax().text_trimmed_range(),
            markup! {
                "Prefer " {symbol_type} " names in camel case."
            },
        );

        Some(diag)
    }

    fn action(ctx: &RuleContext<Self>, State { new_name }: &Self::State) -> Option<JsRuleAction> {
        let model = ctx.model();
        let mut batch = ctx.root().begin();

        let candidates = (2..).map(|i| format!("{}{}", new_name, i).into());
        let candidates = once(Cow::from(new_name)).chain(candidates);

        match ctx.query() {
            JsAnyCamelCaseName::JsIdentifierBinding(binding) => {
                let renamed =
                    batch.rename_node_declaration_with_retry(model, binding.clone(), candidates);
                if renamed {
                    Some(JsRuleAction {
                        category: ActionCategory::Refactor,
                        applicability: Applicability::Always,
                        message: markup! { "Rename this symbol to camel case" }.to_owned(),
                        mutation: batch,
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

declare_node_union! {
    pub(crate) JsAnyCamelCaseName = JsIdentifierBinding | JsLiteralMemberName | JsPrivateClassMemberName
}
