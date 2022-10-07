var Airtable = require('airtable');
var base = new Airtable({ apiKey: process.env.AIRTABLE_API_KEY }).base('appzk3oeSLhSwr9Dd');
const webpush = require('web-push');

// Docs on event and context https://www.netlify.com/docs/functions/#the-handler-method
exports.handler = async (event) => {
  if (event.httpMethod !== "POST") {
    return {
      statusCode: 405
    };
  }

  webpush.setVapidDetails(
    'mailto:hello@kimlaitrinh.me',
    'BPj113A6FPAs0TsDY5xyJTxL_c7XqiCCBWoo_gXfLnju_lBmMuWoKrgP6RG0COAx27DNDOZR6u8e-fodnlUl2qA',
    process.env.VAPID_PRIVATE_KEY
  );

  try {
    const subscriptions = await base('subscriptions').select().all();
    for (const record of subscriptions) {
      try {
        const subscription = JSON.parse(record.get('subscription'));
        await webpush.sendNotification(subscription, "Coucou!");
      } catch (error) {
        console.log("should probably delete this subscription", error);
      }
    }
    return {
      statusCode: 204
    };
  } catch (error) {
    return { statusCode: 500, body: error.toString() }
  }
}
