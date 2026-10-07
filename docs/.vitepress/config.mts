import { defineConfig } from 'vitepress'
import fs from 'node:fs'
import path from 'node:path'

// public address of the site (GitHub Pages): sitemap, canonical URLs, llms.txt
const SITE_URL = 'https://andrea-magni.github.io/MARS/'
const SITE_DESCRIPTION = 'MARS-Curiosity: REST library for Delphi, server and client. JAX-RS style resources, JWT, OpenAPI 3, FireDAC, server-sent events, MCP servers for AI agents; Windows and Linux.'

// sections of llms.txt, in the order of the sidebar
const LLMS_SECTIONS: [string, string][] = [
  ['Guide', 'guide/'],
  ['Server', 'server/'],
  ['Features', 'features/'],
  ['Client', 'client/'],
  ['Reference', 'reference/'],
  ['Demos', 'demos/'],
]
const LLMS_EXTRA = ['release-notes.md']

function splitFrontmatter(md: string): { data: Record<string, string>, body: string } {
  const data: Record<string, string> = {}
  const m = md.match(/^---\r?\n([\s\S]*?)\r?\n---\r?\n/)
  if (!m) return { data, body: md }
  for (const line of m[1].split(/\r?\n/)) {
    const kv = line.match(/^(\w+):\s*(.*)$/)
    if (kv) data[kv[1]] = kv[2].replace(/^['"]|['"]$/g, '')
  }
  return { data, body: md.slice(m[0].length) }
}

// title (first "# " heading) and description (frontmatter, or the first paragraph as plain text)
function pageSummary(md: string): { title: string, description: string } {
  const { data, body } = splitFrontmatter(md)
  const title = data.title || (body.match(/^#\s+(.+)$/m)?.[1] ?? '').trim()
  let description = data.description || ''
  if (!description) {
    const para = body.split(/\r?\n\s*\r?\n/).map(b => b.trim())
      .find(b => b && !/^(#|```|:::|\||<|-\s|\d+\.\s|!\[|\[\[)/.test(b))
    description = (para ?? '')
      .replace(/!\[[^\]]*\]\([^)]*\)/g, '')
      .replace(/\[([^\]]+)\]\([^)]*\)/g, '$1')
      .replace(/[`*_]/g, '')
      .replace(/\s+/g, ' ')
      .trim()
  }
  if (description.length > 300) description = description.slice(0, 297).replace(/\s+\S*$/, '') + '...'
  return { title, description }
}

function pageUrl(relativePath: string): string {
  return SITE_URL + relativePath.replace(/(^|\/)index\.md$/, '$1').replace(/\.md$/, '')
}

// MARS-Curiosity documentation site configuration
export default defineConfig({
  base: '/MARS/',
  title: 'MARS-Curiosity',
  description: SITE_DESCRIPTION,
  lang: 'en-US',
  lastUpdated: true,
  cleanUrls: true,
  ignoreDeadLinks: false,

  // Internal maintenance docs that should not be part of the published site.
  srcExclude: ['REGEN.md', '**/CODE_OF_CONDUCT.md'],

  sitemap: { hostname: SITE_URL },

  head: [
    ['link', { rel: 'icon', href: '/logo-256.png' }],
    ['meta', { name: 'theme-color', content: '#e23c2e' }],
    // Google Search Console ownership (also docs/public/googlea4bcf4b25aec7c8a.html)
    ['meta', { name: 'google-site-verification', content: 'wal16o518WBogF_ELurPnRkWrVusyVZ20TgT8SpWmys' }],
    ['meta', { name: 'keywords', content: 'Delphi, Object Pascal, REST, REST API, REST server, REST client, web API framework, JAX-RS, JWT, OpenAPI, Swagger, FireDAC, server-sent events, MCP, Model Context Protocol, AI agents, Linux, Docker' }],
    ['meta', { property: 'og:type', content: 'website' }],
    ['meta', { property: 'og:site_name', content: 'MARS-Curiosity' }],
    ['meta', { property: 'og:image', content: SITE_URL + 'hero.png' }],
    ['meta', { name: 'twitter:card', content: 'summary' }],
    ['link', { rel: 'alternate', type: 'text/plain', title: 'llms.txt', href: SITE_URL + 'llms.txt' }],
  ],

  // pages without a description in the frontmatter: the first paragraph (meta description)
  transformPageData(pageData, { siteConfig }) {
    if (!pageData.frontmatter.description && pageData.relativePath) {
      const file = path.join(siteConfig.srcDir, pageData.relativePath)
      if (fs.existsSync(file)) {
        const { description } = pageSummary(fs.readFileSync(file, 'utf-8'))
        if (description) pageData.description = description
      }
    }
  },

  // canonical URL and Open Graph / Twitter tags of every page
  transformHead({ pageData, title, description, siteConfig }) {
    const url = pageUrl(pageData.relativePath)
    const extra: any[] = []
    // FAQ: schema.org FAQPage from the "### question" headings and the first paragraph after each
    if (pageData.relativePath === 'guide/faq.md') {
      const body = splitFrontmatter(fs.readFileSync(path.join(siteConfig.srcDir, pageData.relativePath), 'utf-8')).body
      const entities = [...body.matchAll(/^###\s+(.+)\r?\n\r?\n([^\r\n].*)$/gm)].map(m => ({
        '@type': 'Question',
        name: m[1].trim(),
        acceptedAnswer: {
          '@type': 'Answer',
          text: m[2].replace(/\[([^\]]+)\]\([^)]*\)/g, '$1').replace(/[`*]/g, '').trim(),
        },
      }))
      extra.push(['script', { type: 'application/ld+json' }, JSON.stringify({ '@context': 'https://schema.org', '@type': 'FAQPage', mainEntity: entities })])
    }
    return [
      ...extra,
      ['link', { rel: 'canonical', href: url }],
      ['meta', { property: 'og:url', content: url }],
      ['meta', { property: 'og:title', content: title }],
      ['meta', { property: 'og:description', content: description }],
      ['meta', { name: 'twitter:title', content: title }],
      ['meta', { name: 'twitter:description', content: description }],
    ]
  },

  // llms.txt (index of the pages, https://llmstxt.org), llms-full.txt (all the pages in one file)
  // and a plain markdown copy of every page next to its HTML
  buildEnd(siteConfig) {
    const pages = siteConfig.pages.filter(p => LLMS_SECTIONS.some(([, prefix]) => p.startsWith(prefix)) || LLMS_EXTRA.includes(p))
    const read = (p: string) => fs.readFileSync(path.join(siteConfig.srcDir, p), 'utf-8')
    const order = (p: string) => { const i = LLMS_SECTIONS.findIndex(([, prefix]) => p.startsWith(prefix)); return i < 0 ? 99 : i }
    // pages in the order of the sidebar
    const sidebarLinks: string[] = []
    const collect = (items: any[]) => items?.forEach(item => {
      if (item.link) sidebarLinks.push(item.link.replace(/^\//, '').replace(/\/$/, '/index') + '.md')
      if (item.items) collect(item.items)
    })
    Object.values(siteConfig.site.themeConfig.sidebar ?? {}).forEach(groups => collect(groups as any[]))
    const rank = (p: string) => { const i = sidebarLinks.indexOf(p); return i < 0 ? Number.MAX_SAFE_INTEGER : i }
    const header = `# MARS-Curiosity\n\n> ${SITE_DESCRIPTION}\n\n`
      + 'MARS-Curiosity is an open source (MPL 2.0) library to build REST servers and clients with Embarcadero Delphi. '
      + 'Source code, releases and demos: https://github.com/andrea-magni/MARS\n'
    let index = header
    let full = header
    const sections = [...LLMS_SECTIONS.map(([name]) => name), 'More']
    for (const [i, name] of sections.entries()) {
      const inSection = pages.filter(p => order(p) === (i < LLMS_SECTIONS.length ? i : 99))
        .sort((a, b) => rank(a) - rank(b) || a.localeCompare(b))
      if (!inSection.length) continue
      index += `\n## ${name}\n\n`
      for (const p of inSection) {
        const md = read(p)
        const { title, description } = pageSummary(md)
        const mdUrl = pageUrl(p).replace(/\/$/, '/index') + '.md'
        index += `- [${title}](${mdUrl})${description ? ': ' + description : ''}\n`
        const body = splitFrontmatter(md).body
        full += `\n\n---\n\nSource: ${pageUrl(p)}\n\n${body.trim()}\n`
        const out = path.join(siteConfig.outDir, p)
        fs.mkdirSync(path.dirname(out), { recursive: true })
        fs.writeFileSync(out, body)
      }
    }
    fs.writeFileSync(path.join(siteConfig.outDir, 'llms.txt'), index)
    fs.writeFileSync(path.join(siteConfig.outDir, 'llms-full.txt'), full)
  },

  themeConfig: {
    logo: '/logo-256.png',

    nav: [
      { text: 'Guide', link: '/guide/introduction' },
      { text: 'Server', link: '/server/engine' },
      { text: 'Client', link: '/client/overview' },
      { text: 'Demos', link: '/demos/' },
      { text: 'Reference', link: '/reference/attributes' },
      { text: 'Release Notes', link: '/release-notes' },
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
            { text: 'Why MARS?', link: '/guide/why-mars' },
            { text: 'Installation', link: '/guide/installation' },
            { text: 'Your First Server', link: '/guide/getting-started' },
            { text: 'Core Concepts', link: '/guide/core-concepts' },
            { text: 'Deployment', link: '/guide/deployment' },
            { text: 'AI Agent Skills', link: '/guide/agent-skills' },
            { text: 'FAQ', link: '/guide/faq' },
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
            { text: 'MCP Servers (AI Agents)', link: '/features/mcp' },
            { text: 'HTML & Templates', link: '/features/templates' },
            { text: 'Request/Response Logging', link: '/features/logging' },
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
            { text: 'MCP Servers (AI Agents)', link: '/features/mcp' },
            { text: 'HTML & Templates', link: '/features/templates' },
            { text: 'Request/Response Logging', link: '/features/logging' },
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
            { text: 'Logging', link: '/client/logging' },
          ],
        },
      ],
      '/demos/': [
        {
          text: 'Demos',
          items: [
            { text: 'Overview', link: '/demos/' },
            { text: 'Tailwind CSS tutorial', link: '/demos/tailwindcss-tutorial' },
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
      message: 'Released under the Mozilla Public License 2.0.',
      copyright: 'Copyright © 2015-present Andrea Magni',
    },
  },
})
