use crate::format_element::signal::SignalKind;
use crate::prelude::Signal;
use crate::printer::stack::{RestorableStack, Stack};
use crate::FormatElement;
use std::iter::FusedIterator;
use std::marker::PhantomData;

pub(super) trait Queue<'a> {
    type Stack: Stack<&'a [FormatElement]>;

    fn stack(&self) -> &Self::Stack;

    fn stack_mut(&mut self) -> &mut Self::Stack;

    fn top_index(&self) -> usize;

    fn set_top_index(&mut self, index: usize);

    fn pop(&mut self) -> Option<&'a FormatElement> {
        match self.stack().top() {
            Some(top_slice) => {
                // SAFETY: Safe because queue ensures that slices inside `slices` are never empty.
                let top_index = self.top_index();
                let element = &top_slice[top_index];

                if top_index + 1 == top_slice.len() {
                    self.stack_mut().pop().unwrap();
                    self.set_top_index(0);
                } else {
                    self.set_top_index(top_index + 1);
                }

                Some(element)
            }
            None => None,
        }
    }

    fn pop_queue_interned(&mut self) -> Option<&'a FormatElement> {
        while let Some(top) = self.pop() {
            match top {
                FormatElement::Interned(interned) => self.extend_back(interned),
                element => return Some(element),
            }
        }

        None
    }

    fn top_with_interned(&self) -> Option<&'a FormatElement> {
        self.stack()
            .top()
            .map(|top_slice| &top_slice[self.top_index()])
    }

    fn top(&mut self) -> Option<&'a FormatElement> {
        while let Some(top) = self.top_with_interned() {
            match top {
                FormatElement::Interned(interned) => self.extend_back(interned),
                element => return Some(element),
            }
        }

        None
    }

    fn extend_back(&mut self, elements: &'a [FormatElement]) {
        match elements {
            [] => {
                // Don't push empty slices
            }
            slice => {
                let top_index = self.top_index();
                let stack = self.stack_mut();
                if let Some(top) = stack.pop() {
                    stack.push(&top[top_index..])
                }

                stack.push(slice);
                self.set_top_index(0);
            }
        }
    }

    fn pop_slice(&mut self) -> Option<&'a [FormatElement]> {
        self.set_top_index(0);
        self.stack_mut().pop()
    }

    fn skip_content(&mut self, kind: SignalKind)
    where
        Self: Sized,
    {
        let iter = self.iter_content(kind);

        for _ in iter {
            // consume whole iterator until end
        }
    }

    fn iter_content<'q>(&'q mut self, kind: SignalKind) -> QueueContentIterator<'a, 'q, Self>
    where
        Self: Sized,
    {
        QueueContentIterator::new(self, kind)
    }

    fn iter<'q>(&'q mut self) -> QueueIterator<'a, 'q, Self>
    where
        Self: Sized,
    {
        QueueIterator {
            queue: self,
            lifetime: PhantomData,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub(super) struct PrintQueue<'a> {
    slices: Vec<&'a [FormatElement]>,
    top_index: usize,
}

impl<'a> PrintQueue<'a> {
    pub(super) fn new(slice: &'a [FormatElement]) -> Self {
        let slices = match slice {
            [] => Vec::default(),
            slice => vec![slice],
        };

        Self {
            slices,
            top_index: 0,
        }
    }

    pub(super) fn is_empty(&self) -> bool {
        self.slices.is_empty()
    }
}

impl<'a> Queue<'a> for PrintQueue<'a> {
    type Stack = Vec<&'a [FormatElement]>;

    fn stack(&self) -> &Self::Stack {
        &self.slices
    }

    fn stack_mut(&mut self) -> &mut Self::Stack {
        &mut self.slices
    }

    fn top_index(&self) -> usize {
        self.top_index
    }

    fn set_top_index(&mut self, index: usize) {
        self.top_index = index
    }
}

// Instead of popping, moves the index to ensure that no elements are removed from slices.
// This does not work.
#[must_use]
#[derive(Debug)]
pub(super) struct FitsQueue<'a, 'print> {
    stack: RestorableStack<&'a [FormatElement]>,
    top_index: usize,

    print_queue: &'print mut PrintQueue<'a>,
}

impl<'a, 'print> FitsQueue<'a, 'print> {
    pub(super) fn new(
        print_queue: &'print mut PrintQueue<'a>,
        saved: Vec<&'a [FormatElement]>,
    ) -> Self {
        let slices = std::mem::take(&mut print_queue.slices);
        let stack = RestorableStack::new(slices).with_stack(saved);

        Self {
            stack,
            top_index: print_queue.top_index,

            print_queue,
        }
    }

    pub(super) fn finish(mut self) -> Vec<&'a [FormatElement]> {
        let mut stack = self.stack.take_vec();
        stack.clear();

        self.print_queue.slices = self.stack.finish();
        stack
    }
}

impl<'a> Queue<'a> for FitsQueue<'a, '_> {
    type Stack = RestorableStack<&'a [FormatElement]>;

    fn stack(&self) -> &Self::Stack {
        &self.stack
    }

    fn stack_mut(&mut self) -> &mut Self::Stack {
        &mut self.stack
    }

    fn top_index(&self) -> usize {
        self.top_index
    }

    fn set_top_index(&mut self, index: usize) {
        self.top_index = index;
    }
}

pub(super) struct QueueIterator<'a, 'q, Q: Queue<'a>> {
    queue: &'q mut Q,
    lifetime: PhantomData<&'a ()>,
}

impl<'a, Q> Iterator for QueueIterator<'a, '_, Q>
where
    Q: Queue<'a>,
{
    type Item = &'a FormatElement;

    fn next(&mut self) -> Option<Self::Item> {
        self.queue.pop()
    }
}

impl<'a, Q> FusedIterator for QueueIterator<'a, '_, Q> where Q: Queue<'a> {}

pub(super) struct QueueContentIterator<'a, 'q, Q: Queue<'a>> {
    queue: &'q mut Q,
    kind: SignalKind,
    depth: usize,
    lifetime: PhantomData<&'a ()>,
}

impl<'a, 'q, Q> QueueContentIterator<'a, 'q, Q>
where
    Q: Queue<'a>,
{
    fn new(queue: &'q mut Q, kind: SignalKind) -> Self {
        Self {
            queue,
            kind,
            depth: 1,
            lifetime: PhantomData,
        }
    }
}

impl<'a, Q> Iterator for QueueContentIterator<'a, '_, Q>
where
    Q: Queue<'a>,
{
    type Item = &'a FormatElement;

    fn next(&mut self) -> Option<Self::Item> {
        match self.depth {
            0 => None,
            _ => {
                let next = self
                    .queue
                    .pop_queue_interned()
                    .expect("Missing end signal.");

                match next {
                    element @ FormatElement::Signal(signal) if signal.kind() == self.kind => {
                        if signal.is_start() {
                            self.depth += 1;
                        } else {
                            self.depth -= 1;

                            if self.depth == 0 {
                                return None;
                            }
                        }

                        Some(element)
                    }
                    element => Some(element),
                }
            }
        }
    }
}

impl<'a, Q> FusedIterator for QueueContentIterator<'a, '_, Q> where Q: Queue<'a> {}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(super) enum FitsFilterSignal {
    /// Include this element when measuring if the content fits.
    Visit,
    /// Skip over the element, don't measure it.
    Skip,
    /// Measure the content of the element but don't consider any elements after this one (assume the document ends after this element).
    End,
}

impl FitsFilterSignal {
    pub(super) const fn is_end(&self) -> bool {
        matches!(self, FitsFilterSignal::End)
    }
}

pub(super) trait FitsFilter {
    fn filter(&mut self, element: &FormatElement) -> FitsFilterSignal;
}

/// Filter that doesn't filter out any elements
pub(super) struct TakeAllFitsFilter;

impl FitsFilter for TakeAllFitsFilter {
    fn filter(&mut self, _element: &FormatElement) -> FitsFilterSignal {
        FitsFilterSignal::Visit
    }
}

#[derive(Debug, Default)]
pub(super) struct OneEntryQueueFilter {
    depth: usize,
}

impl FitsFilter for OneEntryQueueFilter {
    fn filter(&mut self, element: &FormatElement) -> FitsFilterSignal {
        match element {
            FormatElement::Signal(Signal::StartEntry) => {
                self.depth += 1;
                FitsFilterSignal::Visit
            }
            FormatElement::Signal(Signal::EndEntry) => {
                self.depth -= 1;
                if self.depth == 0 {
                    FitsFilterSignal::End
                } else {
                    FitsFilterSignal::Visit
                }
            }
            FormatElement::Interned(_) => FitsFilterSignal::Visit,
            element if self.depth == 0 => {
                panic!("Expected start of entry, found {element:?}");
            }
            _ => FitsFilterSignal::Visit,
        }
    }
}

/// Queue filter that takes the first two entries
#[derive(Debug, Default)]
pub(super) struct FillSeparatorItemPair {
    second: bool,
    depth: usize,
}

impl FitsFilter for FillSeparatorItemPair {
    fn filter(&mut self, element: &FormatElement) -> FitsFilterSignal {
        match element {
            FormatElement::Signal(Signal::StartEntry) => {
                self.depth += 1;
                FitsFilterSignal::Visit
            }
            FormatElement::Signal(Signal::EndEntry) => {
                self.depth -= 1;

                if self.depth == 0 {
                    if self.second {
                        FitsFilterSignal::End
                    } else {
                        self.second = true;
                        FitsFilterSignal::Visit
                    }
                } else {
                    FitsFilterSignal::Visit
                }
            }
            FormatElement::Signal(Signal::EndFill) if self.second && self.depth == 0 => {
                FitsFilterSignal::End
            }

            FormatElement::Interned(_) => FitsFilterSignal::Visit,
            element if self.depth == 0 => {
                panic!("Expected start of entry, found {element:?}");
            }
            _ => FitsFilterSignal::Visit,
        }
    }
}
