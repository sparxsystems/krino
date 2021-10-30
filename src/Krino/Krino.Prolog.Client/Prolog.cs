using System;
using System.Collections.Generic;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

namespace Krino.Prolog.Client
{
    public class Prolog : IProlog
    {
        private string myBaseAddress;

        public Prolog(string baseAddress)
        {
            myBaseAddress = baseAddress;
        }

        public async Task Add(string statement)
        {
            var response = await HttpWebClient.Post(GetUri("add"), statement);
        }

        public Task Add(IEnumerable<string> statements)
        {
            throw new NotImplementedException();
        }

        public Task Clear() => HttpWebClient.Delete(GetUri("clear"));

        public async Task<bool> Evaluate(string statement)
        {
            var response = await HttpWebClient.Get(GetUri("evaluate"));
            var result = response.StatusCode == HttpStatusCode.OK;

            return result;
        }

        public Task Remove(string statement)
        {
            throw new NotImplementedException();
        }

        private Uri GetUri(string webMethod)
        {
            var result = new UriBuilder(myBaseAddress) { Path = webMethod };
            return result.Uri;
        }


        //private HttpClient CreateHttpClient()
        //{
        //    var httpClient = new HttpClient();
        //    httpClient.BaseAddress = new Uri(myBaseAddress);
        //    //httpClient.DefaultRequestHeaders.Accept.Clear();
        //    httpClient.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));

        //    return httpClient;
        //}
    }
}
