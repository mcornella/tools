use super::signal::Signal;
use crate::format_element::signal::DedentMode;
use crate::prelude::*;
use crate::printer::LineEnding;
use crate::{format, format_args, write};
use crate::{
    BufferExtensions, Format, FormatContext, FormatElement, FormatOptions, FormatResult, Formatter,
    GroupId, IndentStyle, LineWidth, PrinterOptions, TransformSourceMap,
};
use rome_rowan::TextSize;
use std::collections::HashMap;
use std::iter::FusedIterator;
use std::ops::Deref;

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Document {
    elements: Vec<FormatElement>,
}

impl From<Vec<FormatElement>> for Document {
    fn from(elements: Vec<FormatElement>) -> Self {
        Self { elements }
    }
}

impl Deref for Document {
    type Target = [FormatElement];

    fn deref(&self) -> &Self::Target {
        self.elements.as_slice()
    }
}

impl FormatElements for [FormatElement] {
    fn will_break(&self) -> bool {
        use Signal::*;
        let mut ignore_depth = 0usize;

        for element in self {
            match element {
                // Line suffix and best fitting act as boundaries
                // Ignore if any of its content breaks
                FormatElement::Signal(StartLineSuffix | StartBestFitting) => {
                    ignore_depth += 1;
                }
                FormatElement::Signal(EndLineSuffix | EndBestFitting) => {
                    ignore_depth -= 1;
                }
                FormatElement::Interned(interned) if ignore_depth == 0 => {
                    if interned.will_break() {
                        return true;
                    }
                }
                element if ignore_depth == 0 && element.will_break() => {
                    return true;
                }
                _ => continue,
            }
        }

        debug_assert_eq!(ignore_depth, 0, "Unclosed start container");

        false
    }

    fn has_label(&self, expected: LabelId) -> bool {
        self.first()
            .map_or(false, |element| element.has_label(expected))
    }

    fn validate(&self) {
        let panic_with_context = |start_index: usize, end_index: usize, message: &str| {
            let iterator = DocumentIterator::new(self);

            let start_with_context = start_index.saturating_sub(3);
            let end_with_context = end_index - start_with_context + 4;

            let slice: Vec<_> = iterator
                .skip(start_with_context)
                .take(end_with_context)
                .collect();

            panic!("Invalid document structure: {message}:\n{slice:#?}");
        };

        let mut iterator = DocumentIterator::new(self).enumerate().peekable();
        let mut stack = Vec::new();

        while let Some((index, element)) = iterator.next() {
            if let FormatElement::Signal(signal) = element {
                if signal.is_start() {
                    stack.push((index, signal.kind()));
                } else {
                    match stack.pop() {
                        Some((start, start_kind)) => {
                            if start_kind != signal.kind() {
                                panic_with_context(
                                    start,
                                    index,
                                    &std::format!(
                                        "Missing end signal for {start_kind:?}. Expected signal {:?} but actual is {signal:?}",
                                        start_kind.end_signal().kind(),
                                    ),
                                )
                            }
                        }
                        None => panic_with_context(
                            0,
                            index,
                            &std::format!(
                                "Unexpected end signal {signal:?} without matching start signal."
                            ),
                        ),
                    }
                }

                match signal {
                    Signal::StartBestFitting | Signal::StartFill => {
                        match iterator.peek() {
                            Some((_, FormatElement::Signal(Signal::StartEntry))) => {
                                // OK
                            }
                            _ => {
                                panic_with_context(
                                    index,
                                    index + 1,
                                    &std::format!(
                                        "{signal:?} must be directly followed by a {:?} signal.",
                                        Signal::StartEntry
                                    ),
                                );
                            }
                        }
                    }
                    Signal::EndEntry => {
                        match iterator.peek() {
                            Some((
                                _,
                                FormatElement::Signal(
                                    Signal::StartEntry
                                    | Signal::StartMostExpandedEntry
                                    | Signal::EndFill
                                    | Signal::EndBestFitting,
                                ),
                            )) => {
                                // OK
                            }
                            _ => {
                                panic_with_context(
                                    index,
                                    index + 1,
                                    &std::format!(
                                        "{signal:?} must be directly followed by a {:?}, {:?}, {:?}, or {:?} signal.",
                                        Signal::StartEntry,
                                        Signal::StartMostExpandedEntry,
                                        Signal::EndFill,
                                        Signal::EndBestFitting
                                    ),
                                );
                            }
                        }
                    }
                    _ => {
                        // OK
                    }
                }
            }
        }
    }
}

impl std::fmt::Display for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let formatted = format!(IrFormatContext::default(), [self.elements.as_slice()])
            .expect("Formatting not to throw any FormatErrors");

        f.write_str(formatted.print().as_code())
    }
}

#[derive(Clone, Default, Debug)]
struct IrFormatContext {
    /// The interned elements that have been printed to this point
    printed_interned_elements: HashMap<Interned, usize>,
}

impl FormatContext for IrFormatContext {
    type Options = IrFormatOptions;

    fn options(&self) -> &Self::Options {
        &IrFormatOptions
    }

    fn source_map(&self) -> Option<&TransformSourceMap> {
        None
    }
}

#[derive(Debug, Clone, Default)]
struct IrFormatOptions;

impl FormatOptions for IrFormatOptions {
    fn indent_style(&self) -> IndentStyle {
        IndentStyle::Space(2)
    }

    fn line_width(&self) -> LineWidth {
        LineWidth(80)
    }

    fn as_print_options(&self) -> PrinterOptions {
        PrinterOptions {
            tab_width: 2,
            print_width: self.line_width().into(),
            line_ending: LineEnding::LineFeed,
            indent_style: IndentStyle::Space(2),
        }
    }
}

impl Format<IrFormatContext> for &[FormatElement] {
    fn fmt(&self, f: &mut Formatter<IrFormatContext>) -> FormatResult<()> {
        use Signal::*;

        let mut group_ids: Vec<Option<GroupId>> = Vec::new();
        let mut first_element = true;
        let mut in_text = false;

        for element in self.iter() {
            let is_text_element = matches!(element, FormatElement::Space | FormatElement::Text(_));
            let is_start_of_text = is_text_element && !in_text;
            let is_end_of_text = !is_text_element && in_text;

            if is_start_of_text || is_end_of_text {
                write!(f, [text("\"")])?;
            }

            in_text = is_text_element;

            if !first_element && !element.is_end_signal() {
                // Write a separator between two elements
                write!(f, [text(","), soft_line_break_or_space()])?;
            }

            if element.is_start_signal() {
                first_element = true;
                // Wrap each signal in a group
                f.write_element(FormatElement::Signal(StartGroup(None)))?;
            } else if element.is_end_signal() {
                // Write the end of the signals content before the signal formats it's closing content
                write!(f, [text("]")])?;
                write_soft_indent_group_end(f)?;
            }

            match element {
                FormatElement::Space => {
                    write!(f, [text(" ")])?;
                }
                FormatElement::Line(mode) => match mode {
                    LineMode::SoftOrSpace => {
                        write!(f, [text("soft_line_break_or_space")])?;
                    }
                    LineMode::Soft => {
                        write!(f, [text("soft_line_break")])?;
                    }
                    LineMode::Hard => {
                        write!(f, [text("hard_line_break")])?;
                    }
                    LineMode::Empty => {
                        write!(f, [text("empty_line")])?;
                    }
                },
                FormatElement::ExpandParent => {
                    write!(f, [text("expand_parent")])?;
                }
                text @ FormatElement::Text(_) => f.write_element(text.clone())?,

                FormatElement::LineSuffixBoundary => {
                    write!(f, [text("line_suffix_boundary")])?;
                }

                FormatElement::Signal(StartIndent) => {
                    write!(f, [text("indent(")])?;
                }
                FormatElement::Signal(EndIndent) => {
                    write!(f, [text(")")])?;
                }

                FormatElement::Signal(StartDedent(mode)) => {
                    let label = match mode {
                        DedentMode::Level => "dedent",
                        DedentMode::Root => "dedentRoot",
                    };

                    write!(f, [text(label), text("(")])?;
                }
                FormatElement::Signal(EndDedent) => {
                    write!(f, [text(")")])?;
                }

                FormatElement::Signal(StartAlign(signal::Align(count))) => {
                    write!(
                        f,
                        [
                            text("align("),
                            dynamic_text(&count.to_string(), TextSize::default()),
                            text(","),
                            space(),
                        ]
                    )?;
                }
                FormatElement::Signal(EndAlign) => {
                    write!(f, [text(")")])?;
                }

                FormatElement::Signal(StartLineSuffix) => {
                    write!(f, [text("line_suffix(")])?;
                }
                FormatElement::Signal(EndLineSuffix) => {
                    write!(f, [text(")")])?;
                }

                FormatElement::Signal(StartComment) => {
                    write!(f, [text("comment(")])?;
                }
                FormatElement::Signal(EndComment) => {
                    write!(f, [text(")")])?;
                }

                FormatElement::Signal(StartVerbatim(_)) => {
                    write!(f, [text("verbatim(")])?;
                }
                FormatElement::Signal(EndVerbatim) => {
                    write!(f, [text(")")])?;
                }

                FormatElement::Signal(StartGroup(id)) => {
                    write!(f, [text("group(")])?;

                    group_ids.push(*id);

                    if id.is_some() {
                        write_group_soft_indent_start(f)?;
                    }
                }
                FormatElement::Signal(EndGroup) => {
                    if let Some(id) = group_ids.pop().unwrap() {
                        write!(
                            f,
                            [
                                text(","),
                                soft_line_break_or_space(),
                                text("{"),
                                group(&format_args![soft_line_indent_or_space(&format_args![
                                    text("id:"),
                                    space(),
                                    dynamic_text(&std::format!("{id:?}"), TextSize::default()),
                                    soft_line_break_or_space()
                                ])]),
                                text("}, "),
                            ]
                        )?;

                        write_soft_indent_group_end(f)?;
                    }

                    write!(f, [text(")")])?;
                }

                FormatElement::Signal(StartIndentIfGroupBreaks(id)) => {
                    write!(f, [text("indent_if_group_breaks(")])?;

                    group_ids.push(Some(*id));

                    write_group_soft_indent_start(f)?;
                }
                FormatElement::Signal(EndIndentIfGroupBreaks) => {
                    // SAFETY: Safe because every start pushes the group id.
                    let id = group_ids.pop().unwrap().unwrap();

                    write!(
                        f,
                        [
                            text(","),
                            soft_line_break_or_space(),
                            text("{"),
                            group(&format_args![soft_line_indent_or_space(&format_args![
                                text("group-id:"),
                                space(),
                                dynamic_text(&std::format!("{:?}", id), TextSize::default()),
                                soft_line_break_or_space()
                            ])]),
                            text("}")
                        ]
                    )?;

                    write_soft_indent_group_end(f)?;
                }

                FormatElement::Signal(StartConditionalContent(condition)) => {
                    match condition.mode {
                        PrintMode::Flat => {
                            write!(f, [text("if_group_fits_on_line")])?;
                        }
                        PrintMode::Expanded => {
                            write!(f, [text("if_group_breaks")])?;
                        }
                    }

                    write!(f, [text("(")])?;

                    group_ids.push(condition.group_id);

                    if condition.group_id.is_some() {
                        write_group_soft_indent_start(f)?;
                    }
                }
                FormatElement::Signal(EndConditionalContent) => {
                    let group_id = group_ids.pop().unwrap();

                    if let Some(id) = group_id {
                        write!(
                            f,
                            [
                                text(","),
                                soft_line_break_or_space(),
                                text("{"),
                                group(&format_args![soft_line_indent_or_space(&format_args![
                                    text("id:"),
                                    space(),
                                    dynamic_text(&std::format!("{id:?}"), TextSize::default()),
                                    soft_line_break_or_space()
                                ])]),
                                text("}")
                            ]
                        )?;

                        write_soft_indent_group_end(f)?;
                    }

                    write!(f, [text(")")])?;
                }

                FormatElement::Signal(StartLabelled(label_id)) => {
                    write!(
                        f,
                        [
                            text("label(\""),
                            dynamic_text(&std::format!("{label_id:?}"), TextSize::default()),
                            text("\","),
                            space(),
                        ]
                    )?;
                }
                FormatElement::Signal(EndLabelled) => {
                    write!(f, [text(")")])?;
                }

                FormatElement::Signal(StartFill) => {
                    write!(f, [text("fill(")])?;
                }
                FormatElement::Signal(EndFill) => {
                    write!(f, [text(")")])?;
                }

                // Don't write anything for entry as the outer loop will print `[` and `]` around the content
                FormatElement::Signal(StartEntry | StartMostExpandedEntry) => {}
                FormatElement::Signal(EndEntry) => {}

                FormatElement::Signal(StartBestFitting) => {
                    write!(f, [text("best_fitting(")])?;
                }
                FormatElement::Signal(EndBestFitting) => {
                    write!(f, [text(")")])?;
                }

                FormatElement::Interned(interned) => {
                    let interned_elements = &mut f.context_mut().printed_interned_elements;

                    match interned_elements.get(interned).copied() {
                        None => {
                            let index = interned_elements.len();
                            interned_elements.insert(interned.clone(), index);

                            write!(
                                f,
                                [
                                    dynamic_text(
                                        &std::format!("<interned {index}>"),
                                        TextSize::default()
                                    ),
                                    space(),
                                ]
                            )?;

                            interned.deref().fmt(f)?;
                        }
                        Some(reference) => {
                            write!(
                                f,
                                [dynamic_text(
                                    &std::format!("<ref interned *{reference}>"),
                                    TextSize::default()
                                )]
                            )?;
                        }
                    }
                }
            }

            if element.is_start_signal() {
                // Write the start of the signals content
                write_group_soft_indent_start(f)?;
            } else if element.is_end_signal() {
                f.write_element(FormatElement::Signal(EndGroup))?;
            }
        }

        if in_text {
            write!(f, [text("\"")])?;
        }

        debug_assert!(first_element, "Missing end signal");
        debug_assert!(group_ids.is_empty(), "Unpopped group ids");

        Ok(())
    }
}

fn write_group_soft_indent_start<Context>(f: &mut Formatter<Context>) -> FormatResult<()> {
    use Signal::*;

    f.write_elements([
        FormatElement::Signal(StartGroup(None)),
        FormatElement::Signal(StartIndent),
        FormatElement::Line(LineMode::Soft),
    ])
}

fn write_soft_indent_group_end<Context>(f: &mut Formatter<Context>) -> FormatResult<()> {
    use Signal::*;

    f.write_elements([
        FormatElement::Signal(EndIndent),
        FormatElement::Line(LineMode::Soft),
        FormatElement::Signal(EndGroup),
    ])
}

/// Iterator that visits every element, traversing into [FormatElement::Interned] elements
pub struct DocumentIterator<'a> {
    stack: Vec<&'a [FormatElement]>,
    current_index: usize,
}

impl<'a> DocumentIterator<'a> {
    fn new(document: &'a [FormatElement]) -> Self {
        let stack = match document {
            [] => Vec::new(),
            slice => vec![slice],
        };

        Self {
            stack,
            current_index: 0,
        }
    }
}

impl<'a> Iterator for DocumentIterator<'a> {
    type Item = &'a FormatElement;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.stack.last() {
                Some(slice) => {
                    let element = &slice[self.current_index];
                    self.current_index += 1;

                    if self.current_index == slice.len() {
                        self.stack.pop();
                        self.current_index = 0;
                    }

                    if let FormatElement::Interned(interned) = element {
                        if let Some(top) = self.stack.pop() {
                            self.stack.push(&top[self.current_index..]);
                        }

                        self.stack.push(interned.deref());
                        self.current_index = 0;

                        continue;
                    }

                    break Some(element);
                }
                None => break None,
            }
        }
    }
}

impl FusedIterator for DocumentIterator<'_> {}
