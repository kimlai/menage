// https://web.dev/push-notifications-handling-messages/
self.addEventListener('push', function(event) {
  console.log(event);
  event.waitUntil(
    self.registration.showNotification('Ménage', {
      body: event.data.text(),
      icon: '/brain.png'
    })
  );
});
