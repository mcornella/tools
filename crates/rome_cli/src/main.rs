//! This is the main binary of Rome.
//!
//! If you're curious about how to use it, check Rome's [website]
//!
//! [website]: https://rome.tools

use rome_cli::{open_transport, setup_panic_handler, Arguments, CliSession, Termination};
use rome_service::workspace;
use tokio::runtime::Runtime;

#[cfg(target_os = "windows")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn main() -> Result<(), Termination> {
    setup_panic_handler();

    let mut args = Arguments::from_env();

    // If the `--use-server` CLI flag is set, try to open a connection to an
    // existing Rome server socket
    let workspace = if args.contains("--use-server") {
        let runtime = Runtime::new()?;
        match open_transport(runtime)? {
            Some(transport) => workspace::client(transport)?,
            None => return Err(Termination::ServerNotRunning),
        }
    } else {
        workspace::server()
    };

    CliSession::new(&*workspace, args).run()
}
