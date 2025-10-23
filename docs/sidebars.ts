import type { SidebarsConfig } from '@docusaurus/plugin-content-docs';

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */
const sidebars: SidebarsConfig = {
    tutorialSidebar: [
        'installation',
        {
            type: 'category',
            label: 'Operations',
            items: [
                'manual/any-role',
                'manual/oracle-role',
                'manual/requester-role',
                'manual/agent-role'
            ]
        },
        { type: 'category', label: 'Examples', items: ['examples/real-world'] },
        {
            type: 'category',
            label: 'Core Concepts',
            items: ['concepts/architecture', 'concepts/antithesis-interface']
        },
        {
            type: 'category',
            label: 'Advanced Topics',
            items: [
                'advanced/security',
                'advanced/code-design',
                'advanced/secrets-management'
            ]
        }
    ]
};

export default sidebars;
