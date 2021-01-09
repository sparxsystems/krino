using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Attributing;
using Krino.Vertical.Utils.Diagnostic;
using System;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class Parser : IParser
    {
        private IConstructiveDictionary myConstructiveDictionary;

        public Parser(IConstructiveDictionary constructiveDictionary)
        {
            using (Trace.Entering())
            {
                myConstructiveDictionary = constructiveDictionary;
            }
        }

        public IAdTree Deserialize(string text, int maxMorphDistance)
        {
            using (Trace.Entering())
            {
                IAdTree result = null;

                if (!string.IsNullOrEmpty(text))
                {
                    AdTreeBuilder builder = new AdTreeBuilder(myConstructiveDictionary);

                    string normalizedText = Normalize(text);

                    string[] words = normalizedText.ToLowerInvariant().Split(new char[] { }, StringSplitOptions.RemoveEmptyEntries);
                    foreach (string word in words)
                    {
                        builder.AddWord(word, maxMorphDistance);
                    }

                    builder.Purify();

                    result = builder.ActiveAdTrees.FirstOrDefault();
                }

                return result;
            }
        }

        public string Serialize(IAdTree adTree)
        {
            using (Trace.Entering())
            {
                string result = adTree?.Phrase ?? "";
                return result;
            }
        }

        private string Normalize(string s)
        {
            using (Trace.Entering())
            {
                string result = s;

                foreach (Morpheme morpheme in myConstructiveDictionary.NonLexemes)
                {
                    if (!string.IsNullOrEmpty(morpheme.Morph) &&
                        Attributes.U.NonLexeme.PunctuationMark.IsIn(morpheme.Attributes))
                    {
                        result = result.Replace(morpheme.Morph, string.Join("", " ", morpheme.Morph, " "));
                    }
                }

                return result;
            }
        }
    }
}
