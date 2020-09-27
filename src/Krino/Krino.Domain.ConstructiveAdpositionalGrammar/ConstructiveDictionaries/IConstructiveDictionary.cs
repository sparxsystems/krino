using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Graphs;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    /// <summary>
    /// Declares the constructive dictionary as defined in the Constructive Adpositional Grammar book.
    /// </summary>
    public interface IConstructiveDictionary
    {
        /// <summary>
        /// Finds all lexemes which have similar or exactly matching morph.
        /// </summary>
        /// <param name="morph"></param>
        /// <param name="maxDistance">The level of similarity. If 0 then the exact morph is searched.</param>
        /// <returns></returns>
        IEnumerable<Morpheme> FindLexemes(string morph, int maxDistance);

        /// <summary>
        /// Finds all non-lexemes matching the morph.
        /// </summary>
        /// <param name="morph"></param>
        /// <returns></returns>
        IEnumerable<Morpheme> FindNonLexemes(string morph);

        /// <summary>
        /// Finds all possible combinations of morphemes for all words in the phrase.
        /// </summary>
        /// <param name="phrase"></param>
        /// <param name="maxMorphDistance"></param>
        /// <returns></returns>
        IReadOnlyList<IReadOnlyList<IReadOnlyList<Morpheme>>> DecomposePhrase(IEnumerable<string> phrase, int maxMorphDistance);

        /// <summary>
        /// Finds all possible combinations of morphemes for the word.
        /// </summary>
        /// <param name="word"></param>
        /// <param name="maxMorphDistance">The level of similarity. If 0 then the exact morph is searched.</param>
        /// <returns></returns>
        IReadOnlyList<IReadOnlyList<Morpheme>> DecomposeWord(string word, int maxMorphDistance);

        /// <summary>
        /// Finds patterns matching the morpheme (lexeme or non-lexeme).
        /// </summary>
        /// <param name="morpheme"></param>
        /// <returns></returns>
        IEnumerable<Pattern> FindPatterns(Morpheme morpheme);

        /// <summary>
        /// Finds transference patterns which change the grammar character of the morpheme.
        /// </summary>
        /// <remarks>
        /// E.g. it changes stative to adjunctive (A>O).
        /// </remarks>
        /// <param name="morpheme"></param>
        /// <returns></returns>
        IEnumerable<Pattern> FindGrammarCharacterTransferencePatterns(Morpheme morpheme);


        /// <summary>
        /// All non-lexemes.
        /// </summary>
        IEnumerable<Morpheme> NonLexemes { get; }


        /// <summary>
        /// All patterns.
        /// </summary>
        IEnumerable<Pattern> Patterns { get; }

        /// <summary>
        /// Pattern graph.
        /// </summary>
        /// <remarks>
        /// How GrammarCharacters are interconnected via patterns.
        /// GrammarCharacters are nodes and patterns are edges of the graph.
        /// </remarks>
        IDirectedGraph<GrammarCharacter, Pattern> PatternGraph { get; }
    }
}
