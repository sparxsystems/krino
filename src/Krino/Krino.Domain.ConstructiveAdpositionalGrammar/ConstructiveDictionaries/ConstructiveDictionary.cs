using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Graphs;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    /// <summary>
    /// Implements the constructive dictionary.
    /// </summary>
    public class ConstructiveDictionary : IConstructiveDictionary
    {
        private IAttributesModel myAttributesModel;
        private MultiKeyDistinctValueDictionary<string, Morpheme> myLexemes;
        private MultiKeyDistinctValueDictionary<string, Morpheme> myNonLexemes;

        public ConstructiveDictionary(IAttributesModel attributesModel, IEnumerable<Morpheme> morphemes, IEnumerable<Pattern> patterns)
        {
            using (Trace.Entering())
            {
                myAttributesModel = attributesModel;
                morphemes = morphemes ?? Enumerable.Empty<Morpheme>();
                Patterns = patterns ?? Enumerable.Empty<Pattern>();

                InitializeMorphemes(morphemes);
                InitializePatternGraph();
            }
        }


        public IEnumerable<Morpheme> NonLexemes => myNonLexemes.Select(x => x.Value);

        public IEnumerable<Pattern> Patterns { get; private set; }

        public IDirectedGraph<GrammarCharacter, Pattern> PatternGraph { get; private set; }


        public IEnumerable<Morpheme> FindLexemes(string word, int maxDistance)
        {
            using (Trace.Entering())
            {
                IEnumerable<Morpheme> result = Enumerable.Empty<Morpheme>();

                // Try to find exact lexemes.
                if (myLexemes.TryGetValues(word, out ReadOnlySet<Morpheme> lexemes))
                {
                    result = lexemes;
                }

                if (maxDistance > 0)
                {
                    // Also try to find lexemes which have similar morph.
                    IEnumerable<string> similarMorphs = myLexemes.Keys.FindSimilar(word, maxDistance);

                    if (lexemes != null)
                    {
                        // Note: skip morphemes already returned among exact lexemes.
                        result = result.Concat(similarMorphs.SelectMany(x => myLexemes[x].Where(y => !lexemes.Contains(y))));
                    }
                    else
                    {
                        result = result.Concat(similarMorphs.SelectMany(x => myLexemes[x]));
                    }
                }

                return result;
            }
        }

        public IEnumerable<Morpheme> FindNonLexemes(string morph)
        {
            using (Trace.Entering())
            {
                myNonLexemes.TryGetValues(morph, out ReadOnlySet<Morpheme> result);
                return result ?? Enumerable.Empty<Morpheme>();
            }
        }

        public WordDecomposition DecomposeWord(string word, int maxMorphDistance)
        {
            using (Trace.Entering())
            {
                List<WordConstruct> wordConstructs = FindPossibleWordConstructions(word, maxMorphDistance, new List<Morpheme>())
                // Note: as an input parameter there is the list which is filled during the iteration.
                //       Therefore it must be iterated in once - so it must be converted to the list.
                .ToList();

                List<Morpheme> nonLexemes = FindNonLexemes(word).ToList();

                List<WordConstruct> wordCompositions = new List<WordConstruct>();
                if (wordConstructs.Any())
                {
                    wordCompositions.AddRange(wordConstructs);
                }
                if (nonLexemes.Any())
                {
                    var wordConstruct = new WordConstruct(nonLexemes);
                    wordCompositions.Add(wordConstruct);
                }

                var result = new WordDecomposition(word, wordCompositions);
                return result;
            }
        }

        public PhraseDecomposition DecomposePhrase(IEnumerable<string> phrase, int maxMorphDistance)
        {
            using (Trace.Entering())
            {
                List<WordDecomposition> wordDecompositions = new List<WordDecomposition>();

                // Go via each word.
                foreach (string word in phrase)
                {
                    var wordDecomposition = DecomposeWord(word, maxMorphDistance);

                    if (wordDecomposition.Compositions.Any())
                    {
                        wordDecompositions.Add(wordDecomposition);
                    }
                }

                var result = new PhraseDecomposition(wordDecompositions);
                return result;
            }
        }

        public IEnumerable<Pattern> FindPatterns(Morpheme morpheme)
        {
            using (Trace.Entering())
            {
                IEnumerable<Pattern> result = Patterns
                .Where(x => x.MorphemeRule.GrammarCharacter != GrammarCharacter.e &&
                            !x.IsMonoTransference &&
                            x.MorphemeRule.Evaluate(morpheme));
                return result;
            }
        }

        public IEnumerable<Pattern> FindGrammarCharacterTransferencePatterns(Morpheme morpheme)
        {
            using (Trace.Entering())
            {
                IEnumerable<Pattern> result = Patterns
                .Where(x => x.IsMonoTransference && x.RightRule.Evaluate(morpheme));

                return result;
            }
        }

        private void InitializeMorphemes(IEnumerable<Morpheme> morphemes)
        {
            using (Trace.Entering())
            {
                MorphemeEqualityComparer morphemeEqualityComparer = new MorphemeEqualityComparer();
                myLexemes = new MultiKeyDistinctValueDictionary<string, Morpheme>(EqualityComparer<string>.Default, morphemeEqualityComparer);
                myNonLexemes = new MultiKeyDistinctValueDictionary<string, Morpheme>(EqualityComparer<string>.Default, morphemeEqualityComparer);
                foreach (Morpheme morpheme in morphemes)
                {
                    if (morpheme.IsLexeme)
                    {
                        myLexemes.Add(morpheme.Morph, morpheme);
                    }
                    else
                    {
                        myNonLexemes.Add(morpheme.Morph, morpheme);
                    }
                }
            }
        }

        private void InitializePatternGraph()
        {
            using (Trace.Entering())
            {
                // Defines how patterns connect grammar characters.
                PatternGraph = new DirectedGraph<GrammarCharacter, Pattern>();
                PatternGraph.AddVertices(GrammarCharacterExt.GetValues());

                foreach (Pattern pattern in Patterns)
                {
                    // If the pattern connects two grammar characters.
                    bool isEdge = pattern.LeftRule.GrammarCharacter != GrammarCharacter.e && pattern.RightRule.GrammarCharacter != GrammarCharacter.e;
                    if (isEdge)
                    {
                        PatternGraph.AddEdge(pattern.LeftRule.GrammarCharacter, pattern.RightRule.GrammarCharacter, pattern);

                        if (pattern.LeftRule.GrammarCharacter != pattern.RightRule.GrammarCharacter)
                        {
                            PatternGraph.AddEdge(pattern.RightRule.GrammarCharacter, pattern.LeftRule.GrammarCharacter, pattern);
                        }
                    }
                }
            }
        }


        private IEnumerable<WordConstruct> FindPossibleWordConstructions(
            string word,
            int morphDistance,
            List<Morpheme> localSequence)
        {
            using (Trace.Entering())
            {
                // Find if the word is a lexeme.
                IEnumerable<Morpheme> lexemes = FindLexemes(word, morphDistance);
                foreach (Morpheme lexeme in lexemes)
                {
                    localSequence.Add(lexeme);
                    var wordConstruct = new WordConstruct(localSequence.ToList());
                    yield return wordConstruct;
                    localSequence.RemoveAt(localSequence.Count - 1);
                }

                // Find if the word is a lexeme with suffixes.
                IEnumerable<IReadOnlyList<Morpheme>> wordSuffixes = FindLexemeAndItsSuffixes(word, morphDistance, new List<Morpheme>());
                foreach (IReadOnlyList<Morpheme> sequence in wordSuffixes)
                {
                    var wordConstruct = new WordConstruct(localSequence.Concat(sequence.Reverse()).ToList());
                    yield return wordConstruct;
                }

                // Find if the word is a lexeme with prefixes and suffixes.
                for (int i = 1; i < word.Length; ++i)
                {
                    string nonLexeme = word.Substring(0, i);
                    IEnumerable<Morpheme> prefixHomonyms = FindNonLexemes(nonLexeme)
                        .Where(x => myAttributesModel.IsPrefix(x.Attributes));
                    if (prefixHomonyms.Any())
                    {
                        string newWord = word.Substring(i);

                        foreach (Morpheme prefix in prefixHomonyms)
                        {
                            localSequence.Add(prefix);

                            // Try if there are sub-prefixes.
                            IEnumerable<WordConstruct> sequences = FindPossibleWordConstructions(newWord, morphDistance, localSequence);
                            foreach (var sequence in sequences)
                            {
                                var wordConstruct = new WordConstruct(sequence.Morphemes.ToList());
                                yield return wordConstruct;
                            }

                            localSequence.RemoveAt(localSequence.Count - 1);
                        }
                    }
                }
            }
        }

        private IEnumerable<IReadOnlyList<Morpheme>> FindLexemeAndItsSuffixes(
            string word,
            int morphDistance,
            List<Morpheme> localSequence)
        {
            using (Trace.Entering())
            {
                // If there is some suffix.
                if (localSequence.Count > 0)
                {
                    // If the word is a lexeme.
                    IEnumerable<Morpheme> lexemes = FindLexemes(word, morphDistance);
                    foreach (Morpheme lexeme in lexemes)
                    {
                        localSequence.Add(lexeme);
                        yield return localSequence.ToList();
                        localSequence.RemoveAt(localSequence.Count - 1);
                    }
                }

                // Try to find suffixes in the word.
                for (int i = word.Length - 1; i > 0; --i)
                {
                    string nonLexeme = word.Substring(i);
                    IEnumerable<Morpheme> suffixes = FindNonLexemes(nonLexeme)
                        .Where(x => myAttributesModel.IsSuffix(x.Attributes));
                    if (suffixes.Any())
                    {
                        string newWord = word.Substring(0, i);

                        foreach (Morpheme sufix in suffixes)
                        {
                            localSequence.Add(sufix);

                            IEnumerable<IReadOnlyList<Morpheme>> sequences = FindLexemeAndItsSuffixes(newWord, morphDistance, localSequence);
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
