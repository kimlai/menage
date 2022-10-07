var Airtable = require('airtable');
var base = new Airtable({ apiKey: process.env.AIRTABLE_API_KEY }).base(process.env.AIRTABLE_BASE);
const webpush = require('web-push');

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
  const completionData = JSON.parse(event.body);
  const task = await base('tasks').find(completionData.task[0]);

  try {
    const subscriptions = await base('subscriptions').select().all();
    for (const record of subscriptions) {
      try {
        const subscription = JSON.parse(record.get('subscription'));
        await webpush.sendNotification(subscription, `${completionData.user} a complété la tâche "${task.get('name')}"`);
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
    const completion = await base('completions').create(completionData);
    return {
      statusCode: 201,
      body: JSON.stringify(completion._rawJson)
    };
  } catch (error) {
    console.error(error);
    return { statusCode: 500, body: error.toString() }
  }
}
