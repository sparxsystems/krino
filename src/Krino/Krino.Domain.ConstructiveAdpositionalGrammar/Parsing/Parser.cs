using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using System;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class Parser : IParser
    {
        private IConstructiveDictionary myConstructiveDictionary;

        public Parser(IConstructiveDictionary constructiveDictionary)
        {
            myConstructiveDictionary = constructiveDictionary;
        }

        public IAdTree Deserialize(string text)
        {
            IAdTree result = null;

            if (!string.IsNullOrEmpty(text))
            {
                AdTreeBuilder builder = new AdTreeBuilder(myConstructiveDictionary);

                string[] words = text.Split(new char[] { }, StringSplitOptions.RemoveEmptyEntries);
                foreach (string word in words)
                {
                    builder.AddMorph(word);
                }

                result = builder.ActiveAdTrees.FirstOrDefault();
            }

            return result;
        }

        public string Serialize(IAdTree adTree)
        {
            string result = adTree.Phrase;
            return result;
        }
    }
}
