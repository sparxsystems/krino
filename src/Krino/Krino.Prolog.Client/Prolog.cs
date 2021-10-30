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

        public Task Add(string statement)
        {
            throw new NotImplementedException();
        }

        public Task Add(IEnumerable<string> statements)
        {
            throw new NotImplementedException();
        }

        public Task Clear()
        {
            var result = HttpWebClient.Delete(new Uri("http://localhost:8123/clear/"));
            return Task.CompletedTask;

            //using (var httpClient = CreateHttpClient())
            //{
            //    var result = httpClient.DeleteAsync("clear");
            //    return result;
            //}
        }

        public async Task<bool> Evaluate(string statement)
        {
            bool result = false;

            using (var httpClient = CreateHttpClient())
            {
                var response = await httpClient.GetAsync("evaluate");
                result = response.IsSuccessStatusCode;
            }

            return result;
        }

        public Task Remove(string statement)
        {
            throw new NotImplementedException();
        }

        private HttpClient CreateHttpClient()
        {
            var httpClient = new HttpClient();
            httpClient.BaseAddress = new Uri(myBaseAddress);
            //httpClient.DefaultRequestHeaders.Accept.Clear();
            httpClient.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));

            return httpClient;
        }
    }
}
