/*
  Minimal UI behaviour for the MARS TailwindcssDemo.

  The off-canvas sidebar and the user dropdown are driven from here with plain
  DOM calls, so the demo needs no component library. Tailwind Plus Elements can
  replace this file if you hold a licence - see the tutorial for that variant.

  Markup contract (see templates/partials/sidebar.html and topbar.html):
    [data-sidebar-open]   opens  #mobile-sidebar
    [data-sidebar-close]  closes #mobile-sidebar
    [data-menu-button]    toggles the element it points at via aria-controls
*/
(function () {
  'use strict';

  function hit(target, selector) {
    return target instanceof Element ? target.closest(selector) : null;
  }

  /* ---- off-canvas sidebar ------------------------------------------------ */

  function sidebar() {
    return document.getElementById('mobile-sidebar');
  }

  function openSidebar() {
    var panel = sidebar();
    if (!panel) return;
    panel.hidden = false;
    document.documentElement.classList.add('overflow-hidden');
    var focusable = panel.querySelector('a[href], button');
    if (focusable) focusable.focus();
  }

  function closeSidebar() {
    var panel = sidebar();
    if (!panel) return;
    panel.hidden = true;
    document.documentElement.classList.remove('overflow-hidden');
  }

  /* ---- dropdown menus ---------------------------------------------------- */

  function panelFor(button) {
    var id = button.getAttribute('aria-controls');
    return id ? document.getElementById(id) : null;
  }

  function closeMenus(except) {
    var buttons = document.querySelectorAll('[data-menu-button]');
    for (var i = 0; i < buttons.length; i++) {
      if (buttons[i] === except) continue;
      var panel = panelFor(buttons[i]);
      buttons[i].setAttribute('aria-expanded', 'false');
      if (panel) panel.hidden = true;
    }
  }

  function toggleMenu(button) {
    var panel = panelFor(button);
    if (!panel) return;
    var willOpen = panel.hidden;
    closeMenus(button);
    panel.hidden = !willOpen;
    button.setAttribute('aria-expanded', willOpen ? 'true' : 'false');
  }

  /* ---- wiring ------------------------------------------------------------ */

  document.addEventListener('click', function (event) {
    if (hit(event.target, '[data-sidebar-open]')) {
      event.preventDefault();
      openSidebar();
      return;
    }

    if (hit(event.target, '[data-sidebar-close]')) {
      event.preventDefault();
      closeSidebar();
      return;
    }

    var button = hit(event.target, '[data-menu-button]');
    if (button) {
      event.preventDefault();
      toggleMenu(button);
      return;
    }

    // a click anywhere else dismisses any open menu
    if (!hit(event.target, '[data-menu-panel]')) closeMenus(null);
  });

  document.addEventListener('keydown', function (event) {
    if (event.key !== 'Escape') return;
    closeSidebar();
    closeMenus(null);
  });

  // the drawer only exists below xl: if the viewport grows past it, reset state
  window.addEventListener('resize', function () {
    if (window.matchMedia('(min-width: 80rem)').matches) closeSidebar();
  });
}());
