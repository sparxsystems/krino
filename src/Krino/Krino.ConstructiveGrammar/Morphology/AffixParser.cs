using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Diagnostic;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.ConstructiveGrammar.Morphology
{
    internal class AffixParser
    {
        private IMorphology myMorphology;
        private MultiKeyDistinctValueDictionary<string, IMorpheme> myFreeMorphemes;
        private MultiKeyDistinctValueDictionary<string, IMorpheme> myBoundMorphemes;

        public AffixParser(IMorphology morphology, IEnumerable<IMorpheme> morphemes)
        {
            using var _t = Trace.Entering();

            myMorphology = morphology;

            myFreeMorphemes = new MultiKeyDistinctValueDictionary<string, IMorpheme>();
            myBoundMorphemes = new MultiKeyDistinctValueDictionary<string, IMorpheme>();
            foreach (IMorpheme morpheme in morphemes)
            {
                if (GrammarAttributes.Morpheme.Free.IsIn(morpheme.Attributes))
                {
                    myFreeMorphemes.Add(morpheme.Value, morpheme);
                }
                else
                {
                    myBoundMorphemes.Add(morpheme.Value, morpheme);
                }
            }
        }



        public IEnumerable<IWord> ParseWord(string word, int maxMorphDistance, int maxTypoDistance)
        {
            using var _t = Trace.Entering();

            var wordConstructs = FindPossibleWordConstructions(word, maxMorphDistance, maxTypoDistance, new List<IMorpheme>())
                // Note: as an input parameter there is the list which is filled during the iteration.
                //       Therefore it must be iterated in once - so it must be converted to the list.
                .ToList();

            var result = new List<IWord>();
            if (wordConstructs.Any())
            {
                result.AddRange(wordConstructs);
            }

            //var boundMorphemes = FindBoundMorphemes(word).ToList();
            //if (boundMorphemes.Any())
            //{
            //    var wordConstruct = new Word(myMorphology, boundMorphemes);
            //    result.Add(wordConstruct);
            //}

            return result;
        }


        private IEnumerable<IMorpheme> FindFreeMorphemes(string value, int maxDistance)
        {
            using var _t = Trace.Entering();

            var result = Enumerable.Empty<IMorpheme>();

            // Try to find exact morphemes.
            if (myFreeMorphemes.TryGetValues(value, out var freeMorpheme))
            {
                result = freeMorpheme;
            }

            if (maxDistance > 0)
            {
                // Also try to find morphemes which have similar value.
                var similarMorphs = myFreeMorphemes.Keys.FindSimilar(value, maxDistance);

                if (freeMorpheme != null)
                {
                    // Note: skip morphemes already returned among exact lexemes.
                    result = result.Concat(similarMorphs.SelectMany(x => myFreeMorphemes[x].Where(y => !freeMorpheme.Contains(y))));
                }
                else
                {
                    result = result.Concat(similarMorphs.SelectMany(x => myFreeMorphemes[x]));
                }
            }

            return result;
        }

        private IEnumerable<IMorpheme> FindBoundMorphemes(string value)
        {
            using var _t = Trace.Entering();

            myBoundMorphemes.TryGetValues(value, out var result);
            return result ?? Enumerable.Empty<IMorpheme>();
        }


        private IEnumerable<IWord> FindPossibleWordConstructions(string word, int maxMorphDistance, int maxTypoDistance, List<IMorpheme> localSequence)
        {
            using var _t = Trace.Entering();

            // Find if the word is a lexeme.
            var freeMorphemes = FindFreeMorphemes(word, maxMorphDistance);
            foreach (var freeMorpheme in freeMorphemes)
            {
                localSequence.Add(freeMorpheme);
                var wordConstruct = new Word(myMorphology, localSequence.ToList());
                yield return wordConstruct;
                localSequence.RemoveAt(localSequence.Count - 1);
            }

            // Find if the word is a lexeme with suffixes.
            var wordWithSuffixes = FindLexemeAndItsSuffixes(word, maxMorphDistance, new List<IMorpheme>());
            foreach (var sequence in wordWithSuffixes)
            {
                var wordConstruct = new Word(myMorphology, localSequence.Concat(sequence.Reverse()).ToList());
                yield return wordConstruct;
            }

            // Find if the word is a lexeme with prefixes and suffixes.
            for (int i = 1; i < word.Length; ++i)
            {
                string nonLexeme = word.Substring(0, i);
                var prefixHomonyms = FindBoundMorphemes(nonLexeme)
                    .Where(x => GrammarAttributes.Morpheme.Bound.Prefix.IsIn(x.Attributes));
                if (prefixHomonyms.Any())
                {
                    string newWord = word.Substring(i);

                    foreach (Morpheme prefix in prefixHomonyms)
                    {
                        localSequence.Add(prefix);

                        // Try if there are sub-prefixes.
                        var words = FindPossibleWordConstructions(newWord, maxMorphDistance, maxTypoDistance, localSequence);
                        foreach (var wordConstruct in words)
                        {
                            yield return wordConstruct;
                        }

                        localSequence.RemoveAt(localSequence.Count - 1);
                    }
                }
            }
        }

        private IEnumerable<IReadOnlyList<IMorpheme>> FindLexemeAndItsSuffixes(string word, int maxMorphDistance, List<IMorpheme> localSequence)
        {
            using var _t = Trace.Entering();

            // If there are some suffixes.
            if (localSequence.Count > 0)
            {
                // If the word is a lexeme.
                // Note: morphDistance allows to find root words which are modified by suffix morphology rules.
                //       E.g. the word joking has the root word 'joke' and the suffix 'ing'.
                var freeMorphemes = FindFreeMorphemes(word, maxMorphDistance);
                foreach (var freeMorpheme in freeMorphemes)
                {
                    localSequence.Add(freeMorpheme);
                    yield return localSequence.ToList();
                    localSequence.RemoveAt(localSequence.Count - 1);
                }
            }

            // Try to find suffixes in the word.
            for (int i = word.Length - 1; i > 0; --i)
            {
                var nonLexeme = word.Substring(i);
                var suffixes = FindBoundMorphemes(nonLexeme)
                    .Where(x => GrammarAttributes.Morpheme.Bound.Suffix.IsIn(x.Attributes));
                if (suffixes.Any())
                {
                    var newWord = word.Substring(0, i);

                    foreach (Morpheme suffix in suffixes)
                    {
                        localSequence.Add(suffix);

                        var sequences = FindLexemeAndItsSuffixes(newWord, maxMorphDistance, localSequence);
                        foreach (var sequence in sequences)
                        {
                            yield return sequence;
                        }

                        localSequence.RemoveAt(localSequence.Count - 1);
                    }
                }
            }
        }

    }
}
