use crate::format_element::PrintMode;
use crate::{GroupId, TextSize};
#[cfg(debug_assertions)]
use std::any::type_name;
use std::any::TypeId;
use std::num::NonZeroU8;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Signal {
    /// Indents the content one level deeper, see [crate::indent] for documentation and examples.
    StartIndent,
    EndIndent,

    /// Variant of [SignalKind::Indent] that indents content by a number of spaces. For example, `Align(2)`
    /// indents any content following a line break by an additional two spaces.
    ///
    /// Nesting (Aligns)[SignalKind::Align] has the effect that all except the most inner align are handled as (Indent)[SignalKind::Indent].
    StartAlign(Align),
    EndAlign,

    /// Reduces the indention of the specified content either by one level or to the root, depending on the mode.
    /// Reverse operation of `Indent` and can be used to *undo* an `Align` for nested content.
    StartDedent(DedentMode),
    EndDedent,

    /// Creates a logical group where its content is either consistently printed:
    /// * on a single line: Omitting `LineMode::Soft` line breaks and printing spaces for `LineMode::SoftOrSpace`
    /// * on multiple lines: Printing all line breaks
    ///
    /// See [crate::group] for documentation and examples.
    StartGroup(Option<GroupId>),
    EndGroup,

    /// Allows to specify content that gets printed depending on whatever the enclosing group
    /// is printed on a single line or multiple lines. See [crate::if_group_breaks] for examples.
    StartConditionalContent(Condition),
    EndConditionalContent,

    /// Optimized version of [Signal::StartConditionalContent] for the case where some content
    /// should be indented if the specified group breaks.
    StartIndentIfGroupBreaks(GroupId),
    EndIndentIfGroupBreaks,

    /// Concatenates multiple elements together with a given separator printed in either
    /// flat or expanded mode to fill the print width. Expect that the content is a list of alternating
    /// [element, separator] See [crate::Formatter::fill].
    StartFill,
    EndFill,

    /// Entry inside of a [Signal::StartFill]
    StartEntry,
    StartMostExpandedEntry,
    EndEntry,

    /// Delay the printing of its content until the next line break
    StartLineSuffix,
    EndLineSuffix,

    /// Special semantic element letting the printer and formatter know this is
    /// a comment content, and it should only have a limited influence on the
    /// formatting (for instance line breaks contained within will not cause
    /// the parent group to break if this element is at the start of it).
    StartComment,
    EndComment,

    /// A token that tracks tokens/nodes that are printed as verbatim.
    StartVerbatim(VerbatimKind),
    EndVerbatim,

    /// A list of different variants representing the same content. The printer picks the best fitting content.
    /// Line breaks inside of a best fitting don't propagate to parent groups.
    StartBestFitting,
    EndBestFitting,

    /// Special semantic element marking the content with a label.
    /// This does not directly influence how the content will be printed.
    ///
    /// See [crate::labelled] for documentation.
    StartLabelled(LabelId),
    EndLabelled,
}

impl Signal {
    pub const fn is_start(&self) -> bool {
        match self {
            Signal::StartIndent
            | Signal::StartAlign(_)
            | Signal::StartDedent(_)
            | Signal::StartGroup(_)
            | Signal::StartConditionalContent(_)
            | Signal::StartIndentIfGroupBreaks(_)
            | Signal::StartFill
            | Signal::StartEntry
            | Signal::StartMostExpandedEntry
            | Signal::StartLineSuffix
            | Signal::StartComment
            | Signal::StartVerbatim(_)
            | Signal::StartBestFitting
            | Signal::StartLabelled(_)
            | Signal::EndLabelled => true,
            _ => false,
        }
    }

    pub const fn is_end(&self) -> bool {
        !self.is_start()
    }

    pub const fn kind(&self) -> SignalKind {
        use Signal::*;

        match self {
            StartIndent | EndIndent => SignalKind::Indent,
            StartAlign(_) | EndAlign => SignalKind::Align,
            StartDedent(_) | EndDedent => SignalKind::Dedent,
            StartGroup(_) | EndGroup => SignalKind::Group,
            StartConditionalContent(_) | EndConditionalContent => SignalKind::ConditionalContent,
            StartIndentIfGroupBreaks(_) | EndIndentIfGroupBreaks => SignalKind::IndentIfGroupBreaks,
            StartFill | EndFill => SignalKind::Fill,
            StartEntry | StartMostExpandedEntry | EndEntry => SignalKind::Entry,
            StartLineSuffix | EndLineSuffix => SignalKind::LineSuffix,
            StartComment | EndComment => SignalKind::Comment,
            StartVerbatim(_) | EndVerbatim => SignalKind::Verbatim,
            StartBestFitting | EndBestFitting => SignalKind::BestFitting,
            StartLabelled(_) | EndLabelled => SignalKind::Labelled,
        }
    }

    pub fn eq_kind(&self, other: Signal) -> bool {
        self.kind() == other.kind()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SignalKind {
    Indent,
    Align,
    Dedent,
    Group,
    ConditionalContent,
    IndentIfGroupBreaks,
    Fill,
    Entry,
    LineSuffix,
    Comment,
    Verbatim,
    BestFitting,
    Labelled,
}

impl SignalKind {
    pub const fn end_signal(&self) -> Signal {
        use Signal::*;

        match self {
            SignalKind::Indent => EndIndent,
            SignalKind::Align => EndAlign,
            SignalKind::Dedent => EndDedent,
            SignalKind::Group => EndGroup,
            SignalKind::ConditionalContent => EndConditionalContent,
            SignalKind::IndentIfGroupBreaks => EndIndentIfGroupBreaks,
            SignalKind::Fill => EndFill,
            SignalKind::Entry => EndEntry,
            SignalKind::LineSuffix => EndLineSuffix,
            SignalKind::Comment => EndComment,
            SignalKind::Verbatim => EndVerbatim,
            SignalKind::BestFitting => EndBestFitting,
            SignalKind::Labelled => EndLabelled,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DedentMode {
    /// Reduces the indent by a level (if the current indent is > 0)
    Level,

    /// Reduces the indent to the root
    Root,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Condition {
    /// * Flat -> Omitted if the enclosing group is a multiline group, printed for groups fitting on a single line
    /// * Multiline -> Omitted if the enclosing group fits on a single line, printed if the group breaks over multiple lines.
    pub(crate) mode: PrintMode,

    /// The id of the group for which it should check if it breaks or not. The group must appear in the document
    /// before the conditional group (but doesn't have to be in the ancestor chain).
    pub(crate) group_id: Option<GroupId>,
}

impl Condition {
    pub fn new(mode: PrintMode) -> Self {
        Self {
            mode,
            group_id: None,
        }
    }

    pub fn with_group_id(mut self, id: Option<GroupId>) -> Self {
        self.group_id = id;
        self
    }

    pub fn mode(&self) -> PrintMode {
        self.mode
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Align(pub(crate) NonZeroU8);

impl Align {
    pub fn count(&self) -> NonZeroU8 {
        self.0
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct LabelId {
    id: TypeId,
    #[cfg(debug_assertions)]
    label: &'static str,
}

#[cfg(debug_assertions)]
impl std::fmt::Debug for LabelId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.label)
    }
}

#[cfg(not(debug_assertions))]
impl std::fmt::Debug for LabelId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "#{:?}", self.id)
    }
}

impl LabelId {
    pub fn of<T: ?Sized + 'static>() -> Self {
        Self {
            id: TypeId::of::<T>(),
            #[cfg(debug_assertions)]
            label: type_name::<T>(),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum VerbatimKind {
    Unknown,
    Suppressed,
    Verbatim {
        /// the length of the formatted node
        length: TextSize,
    },
}

impl VerbatimKind {
    pub const fn is_unknown(&self) -> bool {
        matches!(self, VerbatimKind::Unknown)
    }
}
