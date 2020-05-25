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

        public IAdTree Deserialize(string text, int maxMorphDistance)
        {
            IAdTree result = null;

            if (!string.IsNullOrEmpty(text))
            {
                AdTreeBuilder builder = new AdTreeBuilder(myConstructiveDictionary);

                string[] words = text.ToLowerInvariant().Split(new char[] { }, StringSplitOptions.RemoveEmptyEntries);
                foreach (string word in words)
                {
                    builder.AddWord(word, maxMorphDistance);
                }

                builder.Purify();

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
