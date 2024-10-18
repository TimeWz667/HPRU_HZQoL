const { description } = require('../../package')

module.exports = {
  /**
   * Ref：https://v1.vuepress.vuejs.org/config/#title
   */
  title: 'Quality-adjusted life year reduced by shingles',
  /**
   * Ref：https://v1.vuepress.vuejs.org/config/#description
   */
  description: description,

  /**
   * Extra tags to be injected to the page HTML `<head>`
   *
   * ref：https://v1.vuepress.vuejs.org/config/#head
   */
  head: [
    ['meta', { name: 'theme-color', content: '#3eaf7c' }],
    ['meta', { name: 'apple-mobile-web-app-capable', content: 'yes' }],
    ['meta', { name: 'apple-mobile-web-app-status-bar-style', content: 'black' }]
  ],

  /**
   * Theme configuration, here is the default theme configuration for VuePress.
   *
   * ref：https://v1.vuepress.vuejs.org/theme/default-theme-config.html
   */
  themeConfig: {
    repo: '',
    editLinks: false,
    docsDir: '',
    editLinkText: '',
    lastUpdated: false,
    nav: [
      {
        text: 'Review',
        link: '/review/',
      },
      {
        text: 'Analysis',
        link: '/analysis/'
      },
      {
        text: 'Projection',
        link: '/projection/'
      }
    ],
    sidebar: {
      '/review/': [
        {
          title: 'Review',
          collapsable: false,
          sidebarDepth: 1,
          children: [
            '',
            'criteria',
            'results'
          ]
        }
      ],
      '/analysis/': [
        {
          title: 'Analysis',
          collapsable: false,
          sidebarDepth: 1,
          children: [
            '',
            'tte',
            'qol'
          ]
        }
      ],
      '/projection/': [
        {
          title: 'Projection',
          collapsable: false,
          children: [
            '',
            'playground'
          ]
        }
      ]
    }
  },

  /**
   * Apply plugins，ref：https://v1.vuepress.vuejs.org/zh/plugin/
   */
  plugins: [
    '@vuepress/plugin-back-to-top',
    '@vuepress/plugin-medium-zoom',
  ]
}
