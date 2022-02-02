using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Diagnostic;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    public class ConstructiveDictionary2 : IConstructiveDictionary2
    {
        private MultiKeyDistinctValueDictionary<string, IMorpheme> myFreeMorphemes;
        private MultiKeyDistinctValueDictionary<string, IMorpheme> myBoundMorphemes;

        public ConstructiveDictionary2(IEnumerable<IMorpheme> morphemes)
        {
            InitializeMorphemes(morphemes);
        }

        public IEnumerable<IMorpheme> FreeMorphemes => throw new NotImplementedException();

        public IEnumerable<IMorpheme> BoundMorphemes => throw new NotImplementedException();

        public IEnumerable<IWord> FindWords(string word, int maxDistance)
        {
            using var _t = Trace.Entering();

            var wordConstructs = FindPossibleWordConstructions(word, maxDistance, new List<IMorpheme>())
                // Note: as an input parameter there is the list which is filled during the iteration.
                //       Therefore it must be iterated in once - so it must be converted to the list.
                .ToList();

            var boundMorphemes = FindBoundMorphemes(word).ToList();

            var result = new List<IWord>();
            if (wordConstructs.Any())
            {
                result.AddRange(wordConstructs);
            }
            if (boundMorphemes.Any())
            {
                var wordConstruct = new Word(boundMorphemes);
                result.Add(wordConstruct);
            }

            return result;
        }


        public IEnumerable<IMorpheme> FindFreeMorphemes(string value, int maxDistance)
        {
            using var _t = Trace.Entering();

            IEnumerable<IMorpheme> result = Enumerable.Empty<IMorpheme>();

            // Try to find exact morphemes.
            if (myFreeMorphemes.TryGetValues(value, out var freeMorpheme))
            {
                result = freeMorpheme;
            }

            if (maxDistance > 0)
            {
                // Also try to find morphemes which have similar value.
                IEnumerable<string> similarMorphs = myFreeMorphemes.Keys.FindSimilar(value, maxDistance);

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

        public IEnumerable<IMorpheme> FindBoundMorphemes(string value)
        {
            using var _t = Trace.Entering();

            myBoundMorphemes.TryGetValues(value, out var result);
            return result ?? Enumerable.Empty<IMorpheme>();
        }


        private void InitializeMorphemes(IEnumerable<IMorpheme> morphemes)
        {
            using var _t = Trace.Entering();

            myFreeMorphemes = new MultiKeyDistinctValueDictionary<string, IMorpheme>();
            myBoundMorphemes = new MultiKeyDistinctValueDictionary<string, IMorpheme>();
            foreach (IMorpheme morpheme in morphemes)
            {
                if (GrammarAttributes.Morpheme.IsFreeMorpheme(morpheme.Attributes))
                {
                    myFreeMorphemes.Add(morpheme.Value, morpheme);
                }
                else
                {
                    myBoundMorphemes.Add(morpheme.Value, morpheme);
                }
            }
        }


        private IEnumerable<IWord> FindPossibleWordConstructions(string word, int morphDistance, List<IMorpheme> localSequence)
        {
            using var _t = Trace.Entering();

            // Find if the word is a lexeme.
            var freeMorphemes = FindFreeMorphemes(word, morphDistance);
            foreach (var freeMorpheme in freeMorphemes)
            {
                localSequence.Add(freeMorpheme);
                var wordConstruct = new Word(localSequence.ToList());
                yield return wordConstruct;
                localSequence.RemoveAt(localSequence.Count - 1);
            }

            // Find if the word is a lexeme with suffixes.
            var wordSuffixes = FindLexemeAndItsSuffixes(word, morphDistance, new List<IMorpheme>());
            foreach (IReadOnlyList<Morpheme> sequence in wordSuffixes)
            {
                var wordConstruct = new Word(localSequence.Concat(sequence.Reverse()).ToList());
                yield return wordConstruct;
            }

            // Find if the word is a lexeme with prefixes and suffixes.
            for (int i = 1; i < word.Length; ++i)
            {
                string nonLexeme = word.Substring(0, i);
                var prefixHomonyms = FindBoundMorphemes(nonLexeme)
                    .Where(x => GrammarAttributes.Morpheme.IsPrefix(x.Attributes));
                if (prefixHomonyms.Any())
                {
                    string newWord = word.Substring(i);

                    foreach (Morpheme prefix in prefixHomonyms)
                    {
                        localSequence.Add(prefix);

                        // Try if there are sub-prefixes.
                        var words = FindPossibleWordConstructions(newWord, morphDistance, localSequence);
                        foreach (var wordConstruct in words)
                        {
                            yield return wordConstruct;
                        }

                        localSequence.RemoveAt(localSequence.Count - 1);
                    }
                }
            }
        }

        private IEnumerable<IReadOnlyList<IMorpheme>> FindLexemeAndItsSuffixes(string word, int morphDistance, List<IMorpheme> localSequence)
        {
            using (Trace.Entering())
            {
                // If there is some suffix.
                if (localSequence.Count > 0)
                {
                    // If the word is a lexeme.
                    var freeMorphemes = FindFreeMorphemes(word, morphDistance);
                    foreach (var morpheme in freeMorphemes)
                    {
                        localSequence.Add(morpheme);
                        yield return localSequence.ToList();
                        localSequence.RemoveAt(localSequence.Count - 1);
                    }
                }

                // Try to find suffixes in the word.
                for (int i = word.Length - 1; i > 0; --i)
                {
                    var nonLexeme = word.Substring(i);
                    var suffixes = FindBoundMorphemes(nonLexeme)
                        .Where(x => GrammarAttributes.Morpheme.IsSuffix(x.Attributes));
                    if (suffixes.Any())
                    {
                        var newWord = word.Substring(0, i);

                        foreach (Morpheme sufix in suffixes)
                        {
                            localSequence.Add(sufix);

                            var sequences = FindLexemeAndItsSuffixes(newWord, morphDistance, localSequence);
                            foreach (IReadOnlyList<Morpheme> sequence in sequences)
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
}
