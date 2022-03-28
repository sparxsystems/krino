using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishDictionary;
using Krino.Domain.EnglishGrammar.Parsing;

namespace Krino.App.Services
{
    public class EnglishGrammarService : IGrammarService
    {
        private IConstructiveDictionary2? myConstructiveDictionary;
        private EnglishMachine? myEnglishGrammar;

        private Task myInitTask;


        public EnglishGrammarService()
        {
            myInitTask = Task.Run(() =>
            {
                myConstructiveDictionary = new EnglishConstructiveDictionaryFactory().Create();
                myEnglishGrammar = new EnglishMachine(false);
            });
        }

        public async Task<Parser> CreateParser()
        {
            await myInitTask;

            var parser = new Parser(myConstructiveDictionary, myEnglishGrammar?.Machine);
            return parser;
        }

    }
}
