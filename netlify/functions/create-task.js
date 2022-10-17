const webpush = require('web-push');
const base = require('./airtable');

webpush.setVapidDetails(
  'mailto:hello@kimlaitrinh.me',
  'BPj113A6FPAs0TsDY5xyJTxL_c7XqiCCBWoo_gXfLnju_lBmMuWoKrgP6RG0COAx27DNDOZR6u8e-fodnlUl2qA',
  process.env.VAPID_PRIVATE_KEY
);

// Docs on event and context https://www.netlify.com/docs/functions/#the-handler-method
exports.handler = async (event) => {
  if (event.httpMethod !== "POST") {
    return {
      statusCode: 405
    };
  }
  const taskData = JSON.parse(event.body);
  delete taskData.id;

  try {
    const subscriptions = await base('subscriptions').select().all();
    for (const record of subscriptions) {
      try {
        const subscription = JSON.parse(record.get('subscription'));
        await webpush.sendNotification(subscription, `nouvelle tâche : ${taskData.name}`);
        console.log("Notification successfully sent")
      } catch (error) {
        console.error("should probably delete this subscription", error);
      }
    }
  } catch (error) {
    // do not fail the request because of failed webpushes.
    console.error(error);
  }

  try {
    const task = await base('tasks').create(taskData);
    return {
      statusCode: 201,
      body: JSON.stringify(task._rawJson)
    };
  } catch (error) {
    console.error(error);
    return { statusCode: 500, body: error.toString() }
  }
}
