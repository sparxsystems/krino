using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Vertical.Utils.Collections;
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
        private List<IPattern> myLexemePatterns;
        private MultiDictionaryUniqueValue<string, IMorpheme> myLexemes;
        private MultiDictionaryUniqueValue<string, IMorpheme> myNonLexemes;

        public ConstructiveDictionary(IEnumerable<IMorpheme> morphemes, IEnumerable<IPattern> patterns)
        {
            Patterns = patterns ?? Enumerable.Empty<IPattern>();

            myLexemePatterns = Patterns.Where(x => !x.MorphemeRule.Equals(MorphemeRule.Epsilon)).ToList();

            InitializeMorphemes(morphemes);
            InitializePatternGraph();
        }

        public IEnumerable<IMorpheme> FindLexemes(string morph)
        {
            myLexemes.TryGetValues(morph, out ISet<IMorpheme> result);
            return result ?? Enumerable.Empty<IMorpheme>();
        }

        public IEnumerable<IMorpheme> FindNonLexemes(string morph)
        {
            myNonLexemes.TryGetValues(morph, out ISet<IMorpheme> result);
            return result ?? Enumerable.Empty<IMorpheme>();
        }

        public IEnumerable<IReadOnlyList<IMorpheme>> FindMorphemeSequences(string word)
        {
            IEnumerable<IReadOnlyList<IMorpheme>> result = FindAllMorphemeSequences(word, new List<IMorpheme>());
            return result;
        }

        public IEnumerable<IPattern> FindMatchingPatterns(IMorpheme lexeme)
        {
            IEnumerable<IPattern> result = myLexemePatterns.Where(x => x.MorphemeRule.IsMatch(lexeme.Morph, lexeme.Attributes));
            return result;
        }

        public IEnumerable<IMorpheme> NonLexemes { get; }

        public IEnumerable<IPattern> Patterns { get; private set; }

        public IDirectedGraph<GrammarCharacter, IPattern> PatternGraph { get; private set; }

        private void InitializeMorphemes(IEnumerable<IMorpheme> morphemes)
        {
            MorphemeEqualityComparer morphemeEqualityComparer = new MorphemeEqualityComparer();
            myLexemes = new MultiDictionaryUniqueValue<string, IMorpheme>(EqualityComparer<string>.Default, morphemeEqualityComparer);
            myNonLexemes = new MultiDictionaryUniqueValue<string, IMorpheme>(EqualityComparer<string>.Default, morphemeEqualityComparer);
            foreach (IMorpheme morpheme in morphemes)
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

        private void InitializePatternGraph()
        {
            GrammarCharacter[] grammarCharacters = Enum.GetValues(typeof(GrammarCharacter)).Cast<GrammarCharacter>().ToArray();

            PatternGraph = new DirectedGraph<GrammarCharacter, IPattern>();
            foreach (GrammarCharacter grammarCharacter in grammarCharacters)
            {
                PatternGraph.AddVertex(grammarCharacter.ToString(), grammarCharacter);
            }

            foreach (IPattern pattern in Patterns)
            {
                // If it is an adposition related rule.
                if (!pattern.LeftRule.Equals(PatternRule.Nothing) && !pattern.RightRule.Equals(PatternRule.Nothing))
                {
                    foreach (GrammarCharacter leftGrammarCharacter in grammarCharacters)
                    {
                        if (pattern.LeftRule.IsMatch(leftGrammarCharacter))
                        {
                            foreach (GrammarCharacter rightGrammarCharacter in grammarCharacters)
                            {
                                if (pattern.RightRule.IsMatch(rightGrammarCharacter))
                                {
                                    PatternGraph.AddEdge(leftGrammarCharacter.ToString(), rightGrammarCharacter.ToString(), pattern);
                                    PatternGraph.AddEdge(rightGrammarCharacter.ToString(), leftGrammarCharacter.ToString(), pattern);
                                }
                            }
                        }
                    }
                }
            }
        }


        private IEnumerable<IReadOnlyList<IMorpheme>> FindAllMorphemeSequences(
            string word,
            List<IMorpheme> localSequence)
        {
            // Find if the word is a lexeme.
            IEnumerable<IMorpheme> lexemes = FindLexemes(word);
            foreach (IMorpheme lexeme in lexemes)
            {
                localSequence.Add(lexeme);
                yield return localSequence.ToList();
                localSequence.RemoveAt(localSequence.Count - 1);
            }

            // Find if the word is a lexeme with suffixes.
            IEnumerable<IReadOnlyList<IMorpheme>> wordSuffixes = FindLexemeAndItsSuffixes(word, new List<IMorpheme>());
            foreach (IReadOnlyList<IMorpheme> sequence in wordSuffixes)
            {
                yield return localSequence.Concat(sequence.Reverse()).ToList();
            }

            // Find if the word is a lexeme with prefixes and suffixes.
            for (int i = 1; i < word.Length; ++i)
            {
                string nonLexeme = word.Substring(0, i);
                IEnumerable<IMorpheme> prefixHomonyms = FindNonLexemes(nonLexeme)
                    .Where(x => Attributes.NonLexeme.Affix.Prefix.IsIn(x.Attributes));
                if (prefixHomonyms.Any())
                {
                    string newWord = word.Substring(i);

                    foreach (IMorpheme prefix in prefixHomonyms)
                    {
                        localSequence.Add(prefix);

                        // Try if there are sub-prefixes.
                        IEnumerable<IReadOnlyList<IMorpheme>> sequences = FindAllMorphemeSequences(newWord, localSequence);
                        foreach (IReadOnlyList<IMorpheme> sequence in sequences)
                        {
                            yield return sequence.ToList();
                        }

                        localSequence.RemoveAt(localSequence.Count - 1);
                    }
                }
            }

        }

        private IEnumerable<IReadOnlyList<IMorpheme>> FindLexemeAndItsSuffixes(
            string word,
            List<IMorpheme> localSequence)
        {
            // If there is some suffix.
            if (localSequence.Count > 0)
            {
                // If the word is a lexeme.
                IEnumerable<IMorpheme> lexemes = FindLexemes(word);
                foreach (IMorpheme lexeme in lexemes)
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
                IEnumerable<IMorpheme> sufixes = FindNonLexemes(nonLexeme)
                    .Where(x => Attributes.NonLexeme.Affix.Suffix.IsIn(x.Attributes));
                if (sufixes.Any())
                {
                    string newWord = word.Substring(0, i);

                    foreach (IMorpheme sufix in sufixes)
                    {
                        localSequence.Add(sufix);

                        IEnumerable<IReadOnlyList<IMorpheme>> sequences = FindLexemeAndItsSuffixes(newWord, localSequence);
                        foreach (IReadOnlyList<IMorpheme> sequence in sequences)
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
