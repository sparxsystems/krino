using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class Parser
    {
        private IConstructiveDictionary2 myDictionary;

        public Parser(IConstructiveDictionary2 dictionary)
        {
            myDictionary = dictionary;
        }

        public IReadOnlyList<ISentence> Parse(string text)
        {
            var result = new List<ISentence>();

            //var linguisticStructureFactory = new LinguisticStructureFactory(myDictionary.AttributesModel);

            //var adTreeCreator = new AdTreeCreator(myDictionary);

            //var sentences = text.ToLowerInvariant()
            //    .Replace(".", " .•").Replace("?", " ?•").Replace("!", " !•")
            //    .Split('•', StringSplitOptions.RemoveEmptyEntries);
            //foreach (var sentenceStr in sentences)
            //{
            //    var words = sentenceStr.Split(new char[] { }, StringSplitOptions.RemoveEmptyEntries);
            //    var adTrees = adTreeCreator.Create(words);

            //    if (adTrees.Count > 0)
            //    {
            //        var bestAdTree = adTrees.GetBest();

            //       // var sentence = linguisticStructureFactory.CreateSentence(bestAdTree);
            //       // result.Add(sentence);
            //    }
            //}

            return result;
        }
    }
}
