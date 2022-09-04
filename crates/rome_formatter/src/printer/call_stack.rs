use crate::format_element::signal::SignalKind;
use crate::format_element::PrintMode;
use crate::printer::stack::{RestorableStack, Stack};
use crate::printer::Indention;
use crate::IndentStyle;
use std::num::NonZeroU8;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub(super) enum StackFrameKind {
    Root,
    Signal(SignalKind),
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub(super) struct StackFrame {
    kind: StackFrameKind,
    args: PrintElementArgs,
}

/// Stores arguments passed to `print_element` call, holding the state specific to printing an element.
/// E.g. the `indent` depends on the token the Printer's currently processing. That's why
/// it must be stored outside of the [PrinterState] that stores the state common to all elements.
///
/// The state is passed by value, which is why it's important that it isn't storing any heavy
/// data structures. Such structures should be stored on the [PrinterState] instead.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(super) struct PrintElementArgs {
    indent: Indention,
    mode: PrintMode,
}

impl PrintElementArgs {
    pub fn new(indent: Indention) -> Self {
        Self {
            indent,
            ..Self::default()
        }
    }

    pub(super) fn mode(&self) -> PrintMode {
        self.mode
    }

    pub(super) fn indention(&self) -> Indention {
        self.indent
    }

    pub fn increment_indent_level(mut self, indent_style: IndentStyle) -> Self {
        self.indent = self.indent.increment_level(indent_style);
        self
    }

    pub fn decrement_indent(mut self) -> Self {
        self.indent = self.indent.decrement();
        self
    }

    pub fn reset_indent(mut self) -> Self {
        self.indent = Indention::default();
        self
    }

    pub fn set_indent_align(mut self, count: NonZeroU8) -> Self {
        self.indent = self.indent.set_align(count);
        self
    }

    pub fn with_print_mode(mut self, mode: PrintMode) -> Self {
        self.mode = mode;
        self
    }
}

impl Default for PrintElementArgs {
    fn default() -> Self {
        Self {
            indent: Indention::Level(0),
            mode: PrintMode::Expanded,
        }
    }
}

pub(super) trait CallStack {
    type Stack: Stack<StackFrame>;

    fn stack(&self) -> &Self::Stack;

    fn stack_mut(&mut self) -> &mut Self::Stack;

    fn pop(&mut self, kind: SignalKind) -> PrintElementArgs {
        let last = self.stack_mut().pop();

        match last {
            Some(StackFrame {
                kind: actual_kind,
                args,
            }) => {
                assert_eq!(actual_kind, StackFrameKind::Signal(kind), "Popped stack frame for kind {kind:?} but actual frame is of kind {actual_kind:?}.");
                args
            }
            None => {
                unreachable!("Pop panics if trying to pop the last element.")
            }
        }
    }

    fn top(&self) -> PrintElementArgs {
        self.stack()
            .top()
            .expect("Expected stack to contain root frame.")
            .args
    }

    fn push(&mut self, kind: SignalKind, args: PrintElementArgs) {
        self.stack_mut().push(StackFrame {
            kind: StackFrameKind::Signal(kind),
            args,
        })
    }
}

#[derive(Debug, Clone)]
pub(super) struct PrintCallStack(Vec<StackFrame>);

impl PrintCallStack {
    pub(super) fn new(args: PrintElementArgs) -> Self {
        Self(vec![StackFrame {
            kind: StackFrameKind::Root,
            args,
        }])
    }
}

impl CallStack for PrintCallStack {
    type Stack = Vec<StackFrame>;

    fn stack(&self) -> &Self::Stack {
        &self.0
    }

    fn stack_mut(&mut self) -> &mut Self::Stack {
        &mut self.0
    }
}

#[must_use]
pub(super) struct FitsCallStack<'print> {
    stack: RestorableStack<StackFrame>,

    print: &'print mut PrintCallStack,
}

impl<'print> FitsCallStack<'print> {
    pub(super) fn new(print: &'print mut PrintCallStack, saved: Vec<StackFrame>) -> Self {
        let frames = std::mem::take(&mut print.0);
        let stack = RestorableStack::new(frames).with_saved(saved);

        Self { stack, print }
    }

    pub(super) fn finish(mut self) -> Vec<StackFrame> {
        let restored = self.stack.restore();

        self.print.0 = restored.stack;
        restored.saved
    }
}

impl CallStack for FitsCallStack<'_> {
    type Stack = RestorableStack<StackFrame>;

    fn stack(&self) -> &Self::Stack {
        &self.stack
    }

    fn stack_mut(&mut self) -> &mut Self::Stack {
        &mut self.stack
    }
}
