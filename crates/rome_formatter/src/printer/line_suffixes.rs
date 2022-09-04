use crate::printer::Indention;
use crate::FormatElement;

#[derive(Debug, Default)]
pub(super) struct LineSuffixes<'a> {
    suffixes: Vec<&'a FormatElement>,
    pending: Option<Indention>,
}

impl<'a> LineSuffixes<'a> {
    pub(super) fn extend<I>(&mut self, indention: Indention, elements: I)
    where
        I: IntoIterator<Item = &'a FormatElement>,
    {
        self.suffixes.extend(elements);

        if !self.suffixes.is_empty() && self.pending.is_none() {
            self.pending = Some(indention);
        }
    }

    pub(super) fn take_pending<'l>(
        &'l mut self,
    ) -> Option<(
        Indention,
        impl Iterator<Item = &'a FormatElement> + DoubleEndedIterator + 'l,
    )> {
        self.pending
            .take()
            .map(|indention| (indention, self.suffixes.drain(..)))
    }

    pub(super) fn has_pending(&self) -> bool {
        self.pending.is_some()
    }
}
