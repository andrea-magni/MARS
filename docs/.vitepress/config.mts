import { defineConfig } from 'vitepress'

// MARS-Curiosity documentation site configuration
export default defineConfig({
  base: '/MARS/',
  title: 'MARS-Curiosity',
  description: 'A lightweight, powerful REST library for Delphi — server and client.',
  lang: 'en-US',
  lastUpdated: true,
  cleanUrls: true,
  ignoreDeadLinks: false,

  // Internal maintenance docs that should not be part of the published site.
  srcExclude: ['REGEN.md', '**/CODE_OF_CONDUCT.md'],

  head: [
    ['link', { rel: 'icon', href: '/logo-256.png' }],
    ['meta', { name: 'theme-color', content: '#e23c2e' }],
  ],

  themeConfig: {
    logo: '/logo-256.png',

    nav: [
      { text: 'Guide', link: '/guide/introduction' },
      { text: 'Server', link: '/server/engine' },
      { text: 'Client', link: '/client/overview' },
      { text: 'Demos', link: '/demos/' },
      { text: 'Reference', link: '/reference/attributes' },
      {
        text: 'Links',
        items: [
          { text: 'GitHub', link: 'https://github.com/andrea-magni/MARS' },
          { text: 'Latest release', link: 'https://github.com/andrea-magni/MARS/releases/latest' },
          { text: 'Forum (Delphi-Praxis)', link: 'https://en.delphipraxis.net/forum/34-mars-curiosity-rest-library/' },
          { text: "Author's blog", link: 'https://www.andreamagni.eu' },
        ],
      },
    ],

    sidebar: {
      '/guide/': [
        {
          text: 'Getting Started',
          items: [
            { text: 'Introduction', link: '/guide/introduction' },
            { text: 'Installation', link: '/guide/installation' },
            { text: 'Your First Server', link: '/guide/getting-started' },
            { text: 'Core Concepts', link: '/guide/core-concepts' },
          ],
        },
      ],
      '/server/': [
        {
          text: 'Server Side',
          items: [
            { text: 'Engine', link: '/server/engine' },
            { text: 'Applications', link: '/server/application' },
            { text: 'Resources & Methods', link: '/server/resources' },
            { text: 'Attributes', link: '/server/attributes' },
            { text: 'Parameters & Injection', link: '/server/injection' },
            { text: 'Content Negotiation', link: '/server/content-negotiation' },
            { text: 'Request Lifecycle', link: '/server/request-lifecycle' },
            { text: 'Error Handling', link: '/server/error-handling' },
          ],
        },
        {
          text: 'Features',
          items: [
            { text: 'Authentication (JWT)', link: '/features/authentication' },
            { text: 'Authorization', link: '/features/authorization' },
            { text: 'FireDAC & Datasets', link: '/features/firedac' },
            { text: 'JSON Serialization', link: '/features/serialization' },
            { text: 'OpenAPI 3 & Swagger', link: '/features/openapi' },
            { text: 'Server-Sent Events', link: '/features/sse' },
            { text: 'HTML & Templates', link: '/features/templates' },
          ],
        },
      ],
      '/features/': [
        {
          text: 'Features',
          items: [
            { text: 'Authentication (JWT)', link: '/features/authentication' },
            { text: 'Authorization', link: '/features/authorization' },
            { text: 'FireDAC & Datasets', link: '/features/firedac' },
            { text: 'JSON Serialization', link: '/features/serialization' },
            { text: 'OpenAPI 3 & Swagger', link: '/features/openapi' },
            { text: 'Server-Sent Events', link: '/features/sse' },
            { text: 'HTML & Templates', link: '/features/templates' },
          ],
        },
      ],
      '/client/': [
        {
          text: 'Client Side',
          items: [
            { text: 'Overview', link: '/client/overview' },
            { text: 'Components', link: '/client/components' },
            { text: 'Calling Resources', link: '/client/resources' },
            { text: 'Authentication', link: '/client/authentication' },
            { text: 'FireDAC Client', link: '/client/firedac' },
          ],
        },
      ],
      '/demos/': [
        {
          text: 'Demos',
          items: [
            { text: 'Overview', link: '/demos/' },
          ],
        },
      ],
      '/reference/': [
        {
          text: 'Reference',
          items: [
            { text: 'Attributes', link: '/reference/attributes' },
            { text: 'Media Types', link: '/reference/media-types' },
            { text: 'Configuration Parameters', link: '/reference/parameters' },
          ],
        },
      ],
    },

    socialLinks: [
      { icon: 'github', link: 'https://github.com/andrea-magni/MARS' },
    ],

    editLink: {
      pattern: 'https://github.com/andrea-magni/MARS/edit/master/docs/:path',
      text: 'Edit this page on GitHub',
    },

    search: {
      provider: 'local',
    },

    footer: {
      message: 'Released under the Apache License 2.0.',
      copyright: 'Copyright © 2015-present Andrea Magni',
    },
  },
})
