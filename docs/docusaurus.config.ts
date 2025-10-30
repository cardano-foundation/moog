import { themes as prismThemes } from 'prism-react-renderer';
import type { Config } from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const config: Config = {
  title: 'Moog',
  tagline: 'Testing Cardano on Cardano with Antithesis',
  favicon: 'img/cardano-ada-logo.svg',

  stylesheets: [
    // Asciinema Player CSS (use the minified version for production)
    'https://unpkg.com/asciinema-player@3.0.0/dist/bundle/asciinema-player.min.css',
    // You can add more CSS links if needed, e.g., another theme or custom file
  ],
  // Set the production url of your site here
  url: process.env.MOOG_SITE_URL ?? 'https://cardano-foundation.github.io/',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: process.env.MOOG_SITE_BASE_URL ?? '/moog',

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'cardano-foundation', // Usually your GitHub org/user name.
  projectName: 'moog', // Usually your repo name.

  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  // markdown with mermaid diagrams support
  markdown: {
    mermaid: true,
  },
  themes: ['@docusaurus/theme-mermaid'],
  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
          editUrl: 'https://github.com/cardano-foundation/moog/tree/main/docs/',
        },
        blog: {
          routeBasePath: '/blog',
          showReadingTime: true,
          feedOptions: {
            type: ['rss', 'atom'],
            xslt: true,
          },
          editUrl: 'https://github.com/cardano-foundation/moog/tree/main/docs/',
          onInlineTags: 'warn',
          onInlineAuthors: 'warn',
          onUntruncatedBlogPosts: 'warn',
        },
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    // Replace with your project's social card
    image: 'img/anti-cli-social-card.jpg',
    metadata: [
      { name: 'description', content: 'Antithesis deterministic simulation testing for Cardano' },
    ],
    navbar: {
      title: 'Moog',
      logo: {
        alt: 'Cardano Logo',
        src: 'img/cardano-ada-logo.svg',
      },
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'tutorialSidebar',
          position: 'left',
          label: 'Documentation',
        },
        { to: '/blog', label: 'Blog', position: 'left' },
        {
          href: 'https://github.com/cardano-foundation/moog',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Docs',
          items: [
            {
              label: 'Getting Started',
              to: '/docs/overview',
            }
          ],
        },
        {
          title: 'Community',
          items: [
            {
              label: 'Discord',
              href: 'https://discord.gg/quVqGUrW',
            },
          ],
        },
        {
          title: 'More',
          items: [
            {
              label: 'Blog',
              to: '/blog',
            },
            {
              label: 'GitHub',
              href: 'https://github.com/cardano-foundation/moog',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Cardano Foundation, Inc. Built with Docusaurus.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
    },
      announcementBar: {
      id: 'support_us',
      content:
          '<strong>Moog is still at an early stage and experimental. Contributions and ideas are <a target="_blank" rel="noopener noreferrer" href="https://github.com/cardano-foundation/moog/">most welcome</a></strong>',
      backgroundColor: '#ff9999',
      textColor: '#091E42',
      isCloseable: false,
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
