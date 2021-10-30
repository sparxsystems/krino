using System;
using System.Net;
using System.Text;

namespace Krino.Prolog.Client
{
    internal static class HttpWebClient
    {
        /// <summary>
        /// Sends the GET request to the HTTP server.
        /// </summary>
        /// <param name="uri">The address including the input parameters.</param>
        /// <returns>Response from the HTTP server.</returns>
        public static HttpWebResponse Get(Uri uri)
        {
            HttpWebResponse aResponse = Request("GET", uri, (byte[])null);
            return aResponse;
        }

        /// <summary>
        /// Sends the POST request to the HTTP server.
        /// </summary>
        /// <param name="uri">The address including the input parameters.</param>
        /// <param name="body">Data for the post request.</param>
        /// <returns>Response from the HTTP server.</returns>
        public static HttpWebResponse Post(Uri uri, string body)
        {
            HttpWebResponse aResult = Request("POST", uri, body);
            return aResult;
        }

        /// <summary>
        /// Sends the POST request to the HTTP server.
        /// </summary>
        /// <param name="uri">The address including the input parameters.</param>
        /// <param name="body">Data for the post request.</param>
        /// <returns>Response from the HTTP server.</returns>
        public static HttpWebResponse Post(Uri uri, byte[] body)
        {
            HttpWebResponse aResponse = Request("POST", uri, body);
            return aResponse;
        }

        /// <summary>
        /// Sends the PUT request to the HTTP server.
        /// </summary>
        /// <param name="uri">The address including the input parameters.</param>
        /// <param name="body">Data for the put request.</param>
        /// <returns>Response from the HTTP server.</returns>
        public static HttpWebResponse Put(Uri uri, string body)
        {
            HttpWebResponse aResponse = Request("PUT", uri, body);
            return aResponse;
        }

        /// <summary>
        /// Sends the PUT request to the HTTP server.
        /// </summary>
        /// <param name="uri">The address including the input parameters.</param>
        /// <param name="body">Data for the put request.</param>
        /// <returns>Response from the HTTP server.</returns>
        public static HttpWebResponse Put(Uri uri, byte[] body)
        {
            HttpWebResponse aResponse = Request("PUT", uri, body);
            return aResponse;
        }

        /// <summary>
        /// Sends the PATCH request to the HTTP server.
        /// </summary>
        /// <param name="uri">The address including the input parameters.</param>
        /// <param name="body">Data for the patch request.</param>
        /// <returns>Response from the HTTP server.</returns>
        public static HttpWebResponse Patch(Uri uri, string body)
        {
            HttpWebResponse aResponse = Request("PATCH", uri, body);
            return aResponse;
        }

        /// <summary>
        /// Sends the PATCH request to the HTTP server.
        /// </summary>
        /// <param name="uri">The address including the input parameters.</param>
        /// <param name="body">Data for the patch request.</param>
        /// <returns>Response from the HTTP server.</returns>
        public static HttpWebResponse Patch(Uri uri, byte[] body)
        {
            HttpWebResponse aResponse = Request("PATCH", uri, body);
            return aResponse;
        }

        /// <summary>
        /// Sends the DELETE request to the HTTP server.
        /// </summary>
        /// <param name="uri">The address including the input parameters.</param>
        /// <returns>Response from the HTTP server.</returns>
        public static HttpWebResponse Delete(Uri uri)
        {
            HttpWebResponse aResponse = Request("DELETE", uri, (byte[])null);
            return aResponse;
        }

        /// <summary>
        /// Sends a request to the HTTP server.
        /// </summary>
        /// <param name="httpMethod">HTTP method specifying the request.</param>
        /// <param name="uri">The address including the input parameters.</param>
        /// <param name="body">The request data.</param>
        /// <returns>Response from the HTTP server.</returns>
        public static HttpWebResponse Request(string httpMethod, Uri uri, string body)
        {
            byte[] aBytes = body != null ? Encoding.UTF8.GetBytes(body) : null;
            HttpWebResponse aResult = Request(httpMethod, uri, aBytes);
            return aResult;
        }

        /// <summary>
        /// Sends a request to the HTTP server.
        /// </summary>
        /// <param name="httpMethod">HTTP method specifying the request.</param>
        /// <param name="uri">The address including the input parameters.</param>
        /// <param name="body">The request data.</param>
        /// <returns>Response from the HTTP server.</returns>
        public static HttpWebResponse Request(string httpMethod, Uri uri, byte[] body)
        {
            HttpWebRequest aRequest = (HttpWebRequest)WebRequest.Create(uri);
            aRequest.Method = httpMethod;

            if (body != null)
            {
                using (var aRequestStream = aRequest.GetRequestStream())
                {
                    aRequestStream.Write(body, 0, body.Length);
                }
            }

            // Sends the message.
            HttpWebResponse aResponse = aRequest.GetResponse() as HttpWebResponse;

            return aResponse;
        }

    }
}
