using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Morphology;
using Krino.ConstructiveGrammar.Syntax;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.ConstructiveGrammar.Dictionary
{
    public class ConstructiveDictionary : IConstructiveDictionary
    {
        private MorphologyParser myMorphologyParser;
        private SyntaxMachine mySyntaxMachine;

        public ConstructiveDictionary(IEnumerable<IMorpheme> morphemes, MultiMachine<LinguisticState, IWord> syntaxRules)
        {
            mySyntaxMachine = new SyntaxMachine(syntaxRules);
            myMorphologyParser = new MorphologyParser(morphemes);
        }

        public IReadOnlyList<IText> Parse(string text)
        {
            _ = Trace.Entering();

            try
            {
                var result = new List<IText>();

                if (!string.IsNullOrEmpty(text))
                {
                    var sentences = text.ToLowerInvariant()
                        .Replace(".", " .•").Replace("?", " ?•").Replace("!", " !•")
                        .Split('•', StringSplitOptions.RemoveEmptyEntries);
                    foreach (var sentenceStr in sentences)
                    {
                        List<List<IWord>> wordAlternatives = new List<List<IWord>>();

                        var words = sentenceStr.Split(new char[] { }, StringSplitOptions.RemoveEmptyEntries);
                        foreach (var wordStr in words)
                        {
                            var foundWords = myMorphologyParser.Parse(wordStr, 0);
                            if (foundWords.Any())
                            {
                                wordAlternatives.Add(foundWords.ToList());
                            }
                            else
                            {
                                // Try to asume it is a noun, adjective or a verb.
                                var assumptions = new List<IWord>()
                        {
                            new Word(wordStr, GrammarAttributes.Morpheme.Free.Lexical.Noun),
                            new Word(wordStr, GrammarAttributes.Morpheme.Free.Lexical.Adjective),
                            new Word(wordStr, GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
                        };

                                wordAlternatives.Add(assumptions);
                            }
                        }

                        var wordVariations = wordAlternatives.GetVariations();
                        foreach (var variation in wordVariations)
                        {
                            mySyntaxMachine.Reset();

                            _ = mySyntaxMachine.DebugView;

                            foreach (var word in variation)
                            {
                                mySyntaxMachine.Add(word);

                                if (!mySyntaxMachine.IsActive)
                                {
                                    break;
                                }
                            }

                            if (mySyntaxMachine.IsActive)
                            {
                                var texts = mySyntaxMachine.GetTexts();
                                result.AddRange(texts);
                            }
                        }
                    }
                }

                return result;
            }
            finally
            {
                mySyntaxMachine.Reset();
            }
        }

    }
}
