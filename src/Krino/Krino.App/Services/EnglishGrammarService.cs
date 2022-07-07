using Krino.ConstructiveGrammar.Dictionary;
using Krino.ConstructiveGrammar.Syntax;
using Krino.EnglishDictionary;
using Krino.EnglishGrammar.Parsing;

namespace Krino.App.Services
{
    public class EnglishGrammarService : IGrammarService
    {
        private IConstructiveDictionary? myConstructiveDictionary;
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
