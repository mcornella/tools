/// A school book stack. Allows adding, removing, and inspecting elements at the back.
pub(super) trait Stack<T> {
    /// Removes the last element if any and returns it
    fn pop(&mut self) -> Option<T>;

    /// Pushes a new element at the back
    fn push(&mut self, value: T);

    /// Returns the last element if any
    fn top(&self) -> Option<&T>;

    /// Returns `true` if the stack is empty
    fn is_empty(&self) -> bool;
}

impl<T> Stack<T> for Vec<T> {
    fn pop(&mut self) -> Option<T> {
        self.pop()
    }

    fn push(&mut self, value: T) {
        self.push(value)
    }

    fn top(&self) -> Option<&T> {
        self.last()
    }

    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

#[derive(Debug, Clone)]
pub(super) struct Restored<T> {
    pub(super) stack: Vec<T>,
    pub(super) saved: Vec<T>,
}

/// A Stack that allows restoring itself to its original state.
#[derive(Debug, Clone)]
pub(super) struct RestorableStack<T> {
    stack: Vec<T>,
    saved: Vec<T>,
    original_length: usize,
}

impl<T> RestorableStack<T> {
    pub(super) fn new(original: Vec<T>) -> Self {
        Self {
            original_length: original.len(),
            stack: original,
            saved: Vec::new(),
        }
    }

    pub(super) fn with_saved(mut self, saved: Vec<T>) -> Self {
        assert!(self.saved.is_empty());
        assert!(saved.is_empty());

        self.saved = saved;
        self
    }

    /// Takes the vector storing the saved elements.
    ///
    /// # Panics
    /// If saved is not empty.
    pub(super) fn take_saved(&mut self) -> Vec<T> {
        assert!(self.saved.is_empty());
        std::mem::take(&mut self.saved)
    }

    /// Restores the stack to its original state.
    pub(super) fn restore(mut self) -> Restored<T> {
        if self.saved.is_empty() {
            self.stack.truncate(self.original_length);
        } else if self.stack.is_empty() {
            self.saved.reverse();
            std::mem::swap(&mut self.stack, &mut self.saved);
        } else {
            self.stack.truncate(self.original_length - self.saved.len());
            let saved = std::mem::take(&mut self.saved);
            self.stack.extend(saved.into_iter().rev());
        }

        assert_eq!(self.original_length, self.stack.len());

        Restored {
            stack: self.stack,
            saved: self.saved,
        }
    }

    pub(super) fn into_vec(self) -> Vec<T> {
        self.stack
    }
}

impl<T> Stack<T> for RestorableStack<T>
where
    T: Copy,
{
    fn pop(&mut self) -> Option<T> {
        let needs_saving = self.stack.len() == self.original_length - self.saved.len();

        match self.stack.pop() {
            Some(element) => {
                if needs_saving {
                    self.saved.push(element);
                }

                Some(element)
            }
            None => None,
        }
    }

    fn push(&mut self, value: T) {
        self.stack.push(value);
    }

    fn top(&self) -> Option<&T> {
        self.stack.last()
    }

    fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use crate::printer::stack::{RestorableStack, Stack};

    #[test]
    fn restore_consumed_stack() {
        let original = vec![1, 2, 3];
        let mut restorable = RestorableStack::new(original);

        restorable.push(4);

        assert_eq!(restorable.pop(), Some(4));
        assert_eq!(restorable.pop(), Some(3));
        assert_eq!(restorable.pop(), Some(2));
        assert_eq!(restorable.pop(), Some(1));
        assert_eq!(restorable.pop(), None);

        let restored = restorable.restore();

        assert_eq!(restored.stack, vec![1, 2, 3]);
    }

    #[test]
    fn restore_partially_consumed_stack() {
        let original = vec![1, 2, 3];
        let mut restorable = RestorableStack::new(original);

        restorable.push(4);

        assert_eq!(restorable.pop(), Some(4));
        assert_eq!(restorable.pop(), Some(3));
        assert_eq!(restorable.pop(), Some(2));
        restorable.push(5);
        restorable.push(6);
        restorable.push(7);

        let restored = restorable.restore();

        assert_eq!(restored.stack, vec![1, 2, 3]);
    }

    #[test]
    fn restore_stack() {
        let original = vec![1, 2, 3];
        let mut restorable = RestorableStack::new(original);

        restorable.push(4);
        restorable.push(5);
        restorable.push(6);
        restorable.push(7);

        assert_eq!(restorable.pop(), Some(7));
        assert_eq!(restorable.pop(), Some(6));
        assert_eq!(restorable.pop(), Some(5));

        let restored = restorable.restore();

        assert_eq!(restored.stack, vec![1, 2, 3]);
    }
}
