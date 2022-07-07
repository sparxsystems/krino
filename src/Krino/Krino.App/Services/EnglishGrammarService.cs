using Krino.ConstructiveGrammar.Dictionary;
using Krino.EnglishDictionary;

namespace Krino.App.Services
{
    public class EnglishGrammarService : IGrammarService
    {
        private IConstructiveDictionary? myConstructiveDictionary;

        private Task myInitTask;


        public EnglishGrammarService()
        {
            myInitTask = Task.Run(() =>
            {
                myConstructiveDictionary = new EnglishConstructiveDictionaryFactory().Create();
            });
        }

        public async Task<IConstructiveDictionary?> GetConstructiveDictionary()
        {
            await myInitTask;

            return myConstructiveDictionary;
        }
    }
}
