const base = require('./airtable');

// Docs on event and context https://www.netlify.com/docs/functions/#the-handler-method
exports.handler = async (event) => {
  if (event.httpMethod !== "POST") {
    return {
      statusCode: 405
    };
  }

  try {
    await base('subscriptions').create({
      "subscription": event.body
    });
    return {
      statusCode: 204
    };
  } catch (error) {
    return { statusCode: 500, body: error.toString() }
  }
}
