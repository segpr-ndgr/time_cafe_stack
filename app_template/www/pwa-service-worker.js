const OFFLINE_VERSION = 1;
const CACHE_NAME = 'offline';

const OFFLINE_URL = 'offline.html';
const OFFLINE_MESSAGE = 'Você está offline.';

// Lista de URLs para armazenar em cache
const urlsToCache = [
  '/www/stylesShiny.css', 
  '/www/logo_app.png', 
  '/www/seplagtransparente_login.png',
  '/www/argos2.png'  
];

self.addEventListener('install', (event) => {
  event.waitUntil((async () => {
    const cache = await caches.open(CACHE_NAME);
    // Adicione os URLs ao cache
    await cache.addAll(urlsToCache);
    // Adicione a página offline ao cache
    await cache.add(new Request(OFFLINE_URL, { cache: 'reload' }));
  })());
});

self.addEventListener('activate', (event) => {
  event.waitUntil((async () => {
    if ('navigationPreload' in self.registration) {
      await self.registration.navigationPreload.enable();
    }
  })());

  self.clients.claim();
});

self.addEventListener('fetch', (event) => {
  if (event.request.mode === 'navigate') {
    event.respondWith((async () => {
      try {
        const preloadResponse = await event.preloadResponse;
        if (preloadResponse) {
          return preloadResponse;
        }
        const networkResponse = await fetch(event.request);
        return networkResponse;
      } catch (error) {
        console.log('Fetch failed; returning offline page instead.', error);
        const cache = await caches.open(CACHE_NAME);
        const cachedResponse = await cache.match(OFFLINE_URL);
        if (cachedResponse) {
          return cachedResponse;
        } else {
          return new Response(OFFLINE_MESSAGE, { headers: { 'Content-Type': 'text/plain' } });
        }
      }
    })());
  }
});
