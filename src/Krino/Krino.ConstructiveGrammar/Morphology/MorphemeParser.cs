using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Diagnostic;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace Krino.ConstructiveGrammar.Morphology
{
    internal class MorphemeParser
    {
        private IMorphology myMorphology;
        private MultiKeyDistinctValueDictionary<string, IMorpheme> myFreeMorphemes;
        private MultiKeyDistinctValueDictionary<string, IMorpheme> myBoundMorphemes;
        private MultiKeyDistinctValueDictionary<string, IMorpheme> myPunctuationMarks;

        private Regex mySplitSentenceItemsRegex;


        public MorphemeParser(IMorphology morphology, IEnumerable<IMorpheme> morphemes)
        {
            using var _t = Trace.Entering();

            myMorphology = morphology;

            myFreeMorphemes = new MultiKeyDistinctValueDictionary<string, IMorpheme>();
            myBoundMorphemes = new MultiKeyDistinctValueDictionary<string, IMorpheme>();
            myPunctuationMarks = new MultiKeyDistinctValueDictionary<string, IMorpheme>();

            foreach (var morpheme in morphemes)
            {
                if (GrammarAttributes.PunctuationMark.IsIn(morpheme.Attributes))
                {
                    myPunctuationMarks.Add(morpheme.Value, morpheme);
                }
                else if (GrammarAttributes.Morpheme.Free.IsIn(morpheme.Attributes))
                {
                    myFreeMorphemes.Add(morpheme.Value, morpheme);

                    // Extract suppletions
                    if (morpheme.Suppletions != null)
                    {
                        foreach (var suppletion in morpheme.Suppletions)
                        {
                            myFreeMorphemes.Add(suppletion.Value, suppletion);
                        }
                    }
                }
                else
                {
                    myBoundMorphemes.Add(morpheme.Value, morpheme);
                }
            }

            string hyphen = null;
            if (myPunctuationMarks.Any(x => GrammarAttributes.PunctuationMark.Hyphen.IsIn(x.Value.Attributes)))
            {
                hyphen = myPunctuationMarks.FirstOrDefault(x => GrammarAttributes.PunctuationMark.Hyphen.IsIn(x.Value.Attributes)).Key;
            }

            var punctuationMarksExceptHyphens = string.Concat(myPunctuationMarks.Where(x => x.Key != hyphen && !GrammarAttributes.PunctuationMark.Hyphen.IsIn(x.Value.Attributes)).Select(x => x.Key));
            var punctuationSameAsHyphenExists = myPunctuationMarks.Any(x => x.Key == hyphen && GrammarAttributes.PunctuationMark.Hyphen.IsIn(x.Value.Attributes));

            string pattern;
            if (hyphen != null && punctuationSameAsHyphenExists)
            {
                pattern = @$"([{punctuationMarksExceptHyphens}])|\s+|(w+{hyphen}w+)";
            }
            else
            {
                pattern = @$"([{punctuationMarksExceptHyphens}])|\s+";
            }

            mySplitSentenceItemsRegex = new Regex(pattern);
        }

        public string[] Split(string text) => mySplitSentenceItemsRegex.Split(text).Where(x => x != "").ToArray();

        public bool IsPunctuationMark(string item) => myPunctuationMarks.ContainsKey(item);

        public bool IsEndOfSentencePunctuationMark(string item)
        {
            myPunctuationMarks.TryGetValues(item, out var morphemes);
            if (morphemes != null)
            {
                var result = morphemes.Any(x => GrammarAttributes.PunctuationMark.Period.IsIn(x.Attributes) || GrammarAttributes.PunctuationMark.ExclamationPoint.IsIn(x.Attributes) || GrammarAttributes.PunctuationMark.QuestionMark.IsIn(x.Attributes));
                return result;
            }

            return false;
        }

        public IEnumerable<IMorpheme> FindPunctuationMark(string item)
        {
            myPunctuationMarks.TryGetValues(item, out var result);
            return result;
        }


        /// <summary>
        /// Parses a word.
        /// </summary>
        /// <param name="word"></param>
        /// <param name="maxMorphDistance">string distance for affixes in the word.</param>
        /// <param name="maxTypoDistance">string distance for a typo of a free morpheme in the word.</param>
        /// <returns></returns>
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

            // TODO: how to recognize root morphemes after they were modified (e.g. letter 'e' was removed) after applying a suffix.

            // Find if the word is a lexeme.
            var freeMorphemes = FindFreeMorphemes(word, maxTypoDistance);
            foreach (var freeMorpheme in freeMorphemes)
            {
                localSequence.Add(freeMorpheme);
                var wordConstruct = new Word(myMorphology, localSequence.ToList());
                yield return wordConstruct;
                localSequence.RemoveAt(localSequence.Count - 1);
            }

            // Find if the word is a lexeme with suffixes.
            var wordWithSuffixes = FindLexemeAndItsSuffixes(word, maxMorphDistance, maxTypoDistance, new List<IMorpheme>());
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

        private IEnumerable<IReadOnlyList<IMorpheme>> FindLexemeAndItsSuffixes(string word, int maxMorphDistance, int maxTypoDistance, List<IMorpheme> localSequence)
        {
            using var _t = Trace.Entering();

            // If there are some suffixes.
            if (localSequence.Count > 0)
            {
                // If the word is a lexeme.
                // Note: morphDistance allows to find root words which are modified by suffix morphology rules.
                //       E.g. the word joking has the root word 'joke' and the suffix 'ing'.
                var freeMorphemes = FindFreeMorphemes(word, maxTypoDistance);
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

                        var sequences = FindLexemeAndItsSuffixes(newWord, maxMorphDistance, maxTypoDistance, localSequence);
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
