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

/// A Stack that allows restoring itself to its original state.
#[derive(Debug, Clone)]
pub(super) struct RestorableStack<T> {
    original: Vec<T>,
    original_top: usize,
    stack: Vec<T>,
}

impl<T> RestorableStack<T> {
    pub(super) fn new(original: Vec<T>) -> Self {
        Self {
            original_top: original.len(),
            original,
            stack: Vec::new(),
        }
    }

    pub(super) fn with_stack(mut self, stack: Vec<T>) -> Self {
        debug_assert!(stack.is_empty());

        self.stack = stack;
        self
    }

    pub(super) fn take_vec(&mut self) -> Vec<T> {
        std::mem::take(&mut self.stack)
    }

    /// Restores the stack to its original state.
    pub(super) fn finish(self) -> Vec<T> {
        self.original
    }
}

impl<T> Stack<T> for RestorableStack<T>
where
    T: Copy,
{
    fn pop(&mut self) -> Option<T> {
        self.stack.pop().or_else(|| {
            if self.original_top == 0 {
                None
            } else {
                self.original_top -= 1;
                Some(self.original[self.original_top])
            }
        })
    }

    fn push(&mut self, value: T) {
        self.stack.push(value);
    }

    fn top(&self) -> Option<&T> {
        self.stack.last().or_else(|| {
            if self.original_top == 0 {
                None
            } else {
                Some(&self.original[self.original_top - 1])
            }
        })
    }

    fn is_empty(&self) -> bool {
        self.original_top == 0 && self.stack.is_empty()
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

        let restored = restorable.finish();

        assert_eq!(restored, vec![1, 2, 3]);
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

        let restored = restorable.finish();

        assert_eq!(restored, vec![1, 2, 3]);
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

        let restored = restorable.finish();

        assert_eq!(restored, vec![1, 2, 3]);
    }
}
