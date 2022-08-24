using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Strings;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace Krino.ConstructiveGrammar.Morphology
{
    internal class MorphologyParser
    {
        private IMorphology myMorphology;
        private MultiKeyDistinctValueDictionary<string, IMorpheme> myFreeMorphemes;
        private MultiKeyDistinctValueDictionary<string, IMorpheme> myPrefixes;
        private MultiKeyDistinctValueDictionary<string, IMorpheme> mySuffixes;
        private MultiKeyDistinctValueDictionary<string, IMorpheme> myPunctuationMarks;

        private Regex mySplitSentenceItemsRegex;


        public MorphologyParser(IMorphology morphology, IEnumerable<IMorpheme> morphemes)
        {
            using var _t = Trace.Entering();

            myMorphology = morphology;

            myFreeMorphemes = new MultiKeyDistinctValueDictionary<string, IMorpheme>();
            myPrefixes = new MultiKeyDistinctValueDictionary<string, IMorpheme>();
            mySuffixes = new MultiKeyDistinctValueDictionary<string, IMorpheme>();
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
                else if (GrammarAttributes.Morpheme.Bound.Prefix.IsIn(morpheme.Attributes))
                {
                    myPrefixes.Add(morpheme.Value, morpheme);
                }
                else if (GrammarAttributes.Morpheme.Bound.Suffix.IsIn(morpheme.Attributes))
                {
                    mySuffixes.Add(morpheme.Value, morpheme);
                }
            }

            string hyphen = null;
            if (myPunctuationMarks.Any(x => GrammarAttributes.PunctuationMark.Hyphen.IsIn(x.Value.Attributes)))
            {
                hyphen = myPunctuationMarks.FirstOrDefault(x => GrammarAttributes.PunctuationMark.Hyphen.IsIn(x.Value.Attributes)).Key;
            }

            var punctuationMarksExceptHyphens = string.Concat(myPunctuationMarks.Where(x => x.Key != hyphen && !GrammarAttributes.PunctuationMark.Hyphen.IsIn(x.Value.Attributes)).Select(x => x.Key));
            var punctuationSameAsHyphenExists = myPunctuationMarks.Any(x => x.Key == hyphen && GrammarAttributes.PunctuationMark.Hyphen.IsIn(x.Value.Attributes));

            if (!string.IsNullOrEmpty(punctuationMarksExceptHyphens))
            {
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

        public IEnumerable<IWord> ParseWord(string word)
        {
            using var _t = Trace.Entering();

            var wordConstructs = FindPossibleWordConstructions(word, new List<IMorpheme>())
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



        private IEnumerable<IMorpheme> FindPrefixes(string value)
        {
            using var _t = Trace.Entering();

            myPrefixes.TryGetValues(value, out var result);
            return result ?? Enumerable.Empty<IMorpheme>();
        }

        private IEnumerable<IMorpheme> FindSuffixes(string value)
        {
            using var _t = Trace.Entering();

            mySuffixes.TryGetValues(value, out var result);
            return result ?? Enumerable.Empty<IMorpheme>();
        }


        private IEnumerable<IWord> FindPossibleWordConstructions(string word, List<IMorpheme> localSequence)
        {
            using var _t = Trace.Entering();

            // Find if the word is a lexeme.
            var freeMorphemes = localSequence != null && localSequence.Any() ? 
                FindFreeMorphemes(word, localSequence.Last()) :
                FindFreeMorphemes(word);

            foreach (var freeMorpheme in freeMorphemes)
            {
                localSequence.Add(freeMorpheme);
                var wordConstruct = new Word(myMorphology, localSequence.ToList());
                yield return wordConstruct;
                localSequence.RemoveAt(localSequence.Count - 1);
            }

            // Find if the word is a lexeme with suffixes.
            var wordWithSuffixes = FindLexemeAndItsSuffixes(word, new List<IMorpheme>());
            foreach (var sequence in wordWithSuffixes)
            {
                var wordConstruct = new Word(myMorphology, localSequence.Concat(sequence.Reverse()).ToList());
                yield return wordConstruct;
            }

            // Find if the word is a lexeme with prefixes and suffixes.
            for (int i = 1; i < word.Length; ++i)
            {
                string prefixStr = word.Substring(0, i);
                var prefixHomonyms = FindPrefixes(prefixStr);
                if (prefixHomonyms.Any())
                {
                    string newWord = word.Substring(i);

                    foreach (Morpheme prefix in prefixHomonyms)
                    {
                        localSequence.Add(prefix);

                        // Try if there are sub-prefixes.
                        var words = FindPossibleWordConstructions(newWord, localSequence);
                        foreach (var wordConstruct in words)
                        {
                            yield return wordConstruct;
                        }

                        localSequence.RemoveAt(localSequence.Count - 1);
                    }
                }
            }
        }

        private IEnumerable<IReadOnlyList<IMorpheme>> FindLexemeAndItsSuffixes(string word, List<IMorpheme> localSequence)
        {
            using var _t = Trace.Entering();

            // If there are some suffixes.
            if (localSequence.Count > 0)
            {
                var freeMorphemes = FindFreeMorphemes(word, localSequence.Last());
                foreach (var freeMorpheme in freeMorphemes)
                {
                    localSequence.Add(freeMorpheme);
                    yield return localSequence.ToList();
                    localSequence.RemoveAt(localSequence.Count - 1);
                }
            }

            // Try to find suffixes inside the word.
            for (int i = word.Length - 1; i > 0; --i)
            {
                var suffixStr = word.Substring(i);
                var suffixHomonyms = FindSuffixes(suffixStr);
                if (suffixHomonyms.Any())
                {
                    var newWord = word.Substring(0, i);

                    foreach (var suffix in suffixHomonyms)
                    {
                        localSequence.Add(suffix);

                        var sequences = FindLexemeAndItsSuffixes(newWord, localSequence);
                        foreach (var sequence in sequences)
                        {
                            yield return sequence;
                        }

                        localSequence.RemoveAt(localSequence.Count - 1);
                    }
                }
            }
        }


        /// <summary>
        /// Finds a free morpheme for the value which was transformed by the affix.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="affix"></param>
        /// <returns></returns>
        private IEnumerable<IMorpheme> FindFreeMorphemes(string value, IMorpheme affix)
        {
            // In case the binding is not specified this is a default function to concatenate affix with a word.
            Func<string, string> concat = val => GrammarAttributes.Morpheme.Bound.Suffix.IsIn(affix.Attributes) ?
                // Suffix
                string.Concat(val, affix.Value) :
                // Prefix
                string.Concat(affix.Value, val);

            var freeMorphemeWithAffix = concat(value);

            // Get free morphemes which can bind with the affix.
            var relevantFreeMorphemes = myFreeMorphemes
                .Where(x => x.Key.StartsWithSameCount(value) > 1 && (affix.Binding == null || affix.Binding.CanBind(new Word(myMorphology, x.Value))));

            // Get those free morphemes which after the transformation gives the same word.
            var result = relevantFreeMorphemes
                .Where(x => affix.Binding != null ?
                    affix.Binding.TransformValue(x.Key) == freeMorphemeWithAffix :
                    concat(x.Key) == freeMorphemeWithAffix)
                .Select(x => x.Value);

            return result;
        }


        private IEnumerable<IMorpheme> FindFreeMorphemes(string value)
        {
            using var _t = Trace.Entering();

            var result = Enumerable.Empty<IMorpheme>();

            // Try to find exact morphemes.
            if (myFreeMorphemes.TryGetValues(value, out var freeMorpheme))
            {
                result = freeMorpheme;
            }

            return result;
        }
    }
}
