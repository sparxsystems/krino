using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class Parser
    {
        private IConstructiveDictionary2 myDictionary;
        private GrammarMachine myGrammarMachine;

        public Parser(IConstructiveDictionary2 dictionary, MultiMachine<LinguisticState, IWord> grammarRules)
        {
            myDictionary = dictionary;
            myGrammarMachine = new GrammarMachine(grammarRules);
        }

        public IReadOnlyList<IText> Parse(string text)
        {
            var result = new List<IText>();

            var sentences = text.ToLowerInvariant()
                .Replace(".", " .•").Replace("?", " ?•").Replace("!", " !•")
                .Split('•', StringSplitOptions.RemoveEmptyEntries);
            foreach (var sentenceStr in sentences)
            {
                List<List<IWord>> wordAlternatives = new List<List<IWord>>();

                var words = sentenceStr.Split(new char[] { }, StringSplitOptions.RemoveEmptyEntries);
                foreach (var wordStr in words)
                {
                    // Try to find the word in the dictionary.
                    var maxDistance = wordStr.Length <= 2 ? 0 : 1;
                    var foundWords = myDictionary.FindWords(wordStr, maxDistance);
                    wordAlternatives.Add(foundWords.ToList());
                }

                var wordVariations = wordAlternatives.GetVariations();
                foreach (var variation in wordVariations)
                {
                    myGrammarMachine.Reset();

                    _ = myGrammarMachine.DebugView;

                    foreach (var word in variation)
                    {
                        myGrammarMachine.Add(word);

                        if (!myGrammarMachine.IsActive)
                        {
                            break;
                        }
                    }

                    if (myGrammarMachine.IsActive)
                    {
                        var texts = myGrammarMachine.GetTexts();
                        result.AddRange(texts);
                    }
                }
            }

            return result;
        }
    }
}
