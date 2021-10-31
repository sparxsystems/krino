using Krino.Vertical.Utils.Serializers;
using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Threading.Tasks;

namespace Krino.Prolog.Client
{
    public class Prolog : IProlog, IDisposable
    {
        private ISerializer mySerializer;
        private HttpClient myHttpClient;

        public Prolog(string baseAddress)
        {
            mySerializer = new DataContractJsonStringSerializer();

            myHttpClient = new HttpClient();
            myHttpClient.BaseAddress = new Uri(baseAddress);
            myHttpClient.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
        }

        public void Dispose()
        {
            myHttpClient.Dispose();
        }

        public Task Add(string statement) => Add(new string[] { statement });

        public async Task Add(IEnumerable<string> statements)
        {
            var statementsStr = (string)mySerializer.Serialize(statements);
            var content = new StringContent(statementsStr);
            content.Headers.ContentType = new MediaTypeHeaderValue("application/json");
            var response = await myHttpClient.PostAsync("add", content);
            var s = await response.Content.ReadAsStringAsync();
        }

        public Task Remove(string statement) => Remove(new string[] { statement });

        public async Task Remove(IEnumerable<string> statements)
        {
            var statementsStr = (string)mySerializer.Serialize(statements);
            var content = new StringContent(statementsStr);
            content.Headers.ContentType = new MediaTypeHeaderValue("application/json");
            var response = await myHttpClient.PostAsync("remove", content);
            var s = await response.Content.ReadAsStringAsync();
        }

        public Task Clear() => throw new NotImplementedException();

        public async Task<bool> Evaluate(string statement)
        {
            var statementsStr = (string)mySerializer.Serialize(statement);
            var content = new StringContent(statementsStr);
            content.Headers.ContentType = new MediaTypeHeaderValue("application/json");
            var response = await myHttpClient.PostAsync("evaluate", content);
            var s = await response.Content.ReadAsStringAsync();

            return true;
        }

        
    }
}
