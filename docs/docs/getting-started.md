---
sidebar_position: 1
---

# Getting Started

Welcome to Anti-CLI, the command-line interface for testing Cardano with Antithesis.

## Quick Start

Anti-CLI provides a streamlined way to submit and manage Cardano tests on the Antithesis platform. Follow these steps to get started:

### Installation

1. Download the latest release from the [GitHub releases page](https://github.com/cardano-foundation/antithesis/releases)
2. Extract the binary and add it to your PATH
3. Verify installation: `anti --version`

### Basic Usage

Anti-CLI operates through three main roles:

- **Oracle**: Validates test requests and manages the testing pipeline
- **Requester**: Submits test configurations and requests
- **Agent**: Executes tests and publishes results

### Your First Test

1. **Configure your environment**:
   ```bash
   anti config init
   ```

2. **Submit a test request**:
   ```bash
   anti request submit --config my-test-config.json
   ```

3. **Monitor test progress**:
   ```bash
   anti status check --test-id <your-test-id>
   ```

## Next Steps

- Learn about the [Antithesis Interface](./antithesis-interface.md)
- Understand the different [roles](./oracle-role.md) in the system
- Explore [real-world examples](./real-world.md)

## Need Help?

- Check out our [documentation](./antithesis-interface.md)
- Join our [Discord community](https://discord.gg/quVqGUrW)
- Report issues on [GitHub](https://github.com/cardano-foundation/antithesis/issues)
