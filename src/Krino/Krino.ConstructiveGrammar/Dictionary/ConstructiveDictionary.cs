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
        private MorphologyParser myMorphemeParser;
        private IMorphology myMorphology;
        private SyntaxParser mySyntaParser;

        public ConstructiveDictionary(IMorphology morphology, MultiMachine<LinguisticState, IWord> syntax, IEnumerable<IMorpheme> morphemes)
        {
            myMorphology = morphology;
            mySyntaParser = new SyntaxParser(syntax);
            myMorphemeParser = new MorphologyParser(myMorphology, morphemes);
        }

        public IReadOnlyList<IText> Parse(string text)
        {
            _ = Trace.Entering();

            try
            {
                var result = new List<IText>();

                if (!string.IsNullOrEmpty(text))
                {
                    var textItems = myMorphemeParser.Split(text.ToLowerInvariant());
                    var sentences = textItems.Split(x => myMorphemeParser.IsEndOfSentencePunctuationMark(x));

                    foreach (var sentence in sentences)
                    {
                        List<List<IWord>> wordAlternatives = new List<List<IWord>>();

                        foreach (var sentenceItem in sentence)
                        {
                            if (!myMorphemeParser.IsPunctuationMark(sentenceItem))
                            {
                                var foundWords = myMorphemeParser.ParseWord(sentenceItem);
                                if (foundWords.Any())
                                {
                                    wordAlternatives.Add(foundWords.ToList());
                                }
                                // An uknown word.
                                else
                                {
                                    // Try to asume it is a noun, adjective or a verb.
                                    var assumptions = new List<IWord>()
                                    {
                                        new Word(myMorphology, sentenceItem, GrammarAttributes.Morpheme.Free.Lexical.Noun),
                                        new Word(myMorphology, sentenceItem, GrammarAttributes.Morpheme.Free.Lexical.Adjective),
                                        new Word(myMorphology, sentenceItem, GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
                                    };

                                    wordAlternatives.Add(assumptions);
                                }
                            }
                            else if (myMorphemeParser.IsEndOfSentencePunctuationMark(sentenceItem))
                            {
                                var punctuationMark = myMorphemeParser.FindPunctuationMark(sentenceItem).First();
                                wordAlternatives.Add(new List<IWord> { new Word(myMorphology, punctuationMark) });
                            }
                        }


                        var wordVariations = wordAlternatives.GetVariations();
                        foreach (var wordVariation in wordVariations)
                        {
                            mySyntaParser.Reset();

                            _ = mySyntaParser.DebugView;

                            foreach (var word in wordVariation)
                            {
                                mySyntaParser.Add(word);

                                if (!mySyntaParser.IsActive)
                                {
                                    break;
                                }
                            }

                            if (mySyntaParser.IsActive)
                            {
                                var texts = mySyntaParser.GetTexts();
                                result.AddRange(texts);
                            }
                        }
                    }
                }

                return result;
            }
            finally
            {
                mySyntaParser.Reset();
            }
        }

    }
}
