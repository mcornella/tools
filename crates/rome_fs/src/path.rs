//! This module is responsible to manage paths inside Rome.
//! It is a small wrapper around [path::PathBuf] but it is also able to
//! give additional information around the the file that holds:
//! - the [FileHandlers] for the specific file
//! - shortcuts to open/write to the file
use std::fs::read_to_string;
use std::io::Read;
use std::{fs::File, io, io::Write, ops::Deref, path::PathBuf};

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Serialize, serde::Deserialize, schemars::JsonSchema)
)]
pub struct RomePath {
    path: PathBuf,
    id: usize,
}

impl Deref for RomePath {
    type Target = PathBuf;

    fn deref(&self) -> &Self::Target {
        &self.path
    }
}

impl RomePath {
    pub fn new(path_to_file: impl Into<PathBuf>, id: usize) -> Self {
        Self {
            path: path_to_file.into(),
            id,
        }
    }

    // TODO: handle error with diagnostic?
    /// Opens a file and returns a [File] in write mode
    pub fn open(&self) -> File {
        File::open(&self.path).expect("cannot open the file to format")
    }

    /// Accepts a file opened in read mode and saves into it
    pub fn save(&mut self, content: &str) -> Result<(), std::io::Error> {
        let mut file_to_write = File::create(&self.path).unwrap();
        // TODO: handle error with diagnostic
        file_to_write.write_all(content.as_bytes())
    }

    /// Returns the contents of a file, if it exists
    pub fn get_buffer_from_file(&mut self) -> String {
        let mut file = self.open();
        let mut buffer = String::new();
        // we assume we have permissions
        file.read_to_string(&mut buffer)
            .expect("cannot read the file to format");

        buffer
    }

    /// Small wrapper for [read_to_string]
    pub fn read_to_string(&self) -> io::Result<String> {
        let path = self.path.as_path();
        read_to_string(path)
    }

    /// Retrieves the ID assigned to the file
    pub fn file_id(&self) -> usize {
        self.id
    }

    pub fn extension_as_str(&self) -> &str {
        self.extension()
            .expect("Can't read the file")
            .to_str()
            .expect("Can't read the file")
    }
}
