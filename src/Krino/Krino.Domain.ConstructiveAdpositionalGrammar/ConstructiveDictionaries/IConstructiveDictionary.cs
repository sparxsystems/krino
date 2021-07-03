using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
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
        /// Returns the attributes model for reading morpheme structural and semantic attributes.
        /// </summary>
        IAttributesModel AttributesModel { get; }

        /// <summary>
        /// All lexemes in the dictionary.
        /// </summary>
        IEnumerable<Morpheme> Lexemes { get; }

        /// <summary>
        /// All non-lexemes in the dictionary.
        /// </summary>
        IEnumerable<Morpheme> NonLexemes { get; }

        /// <summary>
        /// Finds all lexemes which have similar or exactly matching word.
        /// </summary>
        /// <param name="word">A word for which lexemes shall be found.</param>
        /// <param name="maxDistance">The level of similarity. If 0 then the exact morph is searched.</param>
        /// <returns></returns>
        IEnumerable<Morpheme> FindLexemes(string word, int maxDistance);

        /// <summary>
        /// Finds all non-lexemes matching the provided word.
        /// </summary>
        /// <param name="word">A "word" for which lexemes shall be found.</param>
        /// <returns></returns>
        IEnumerable<Morpheme> FindNonLexemes(string word);

        /// <summary>
        /// Finds all possible combinations of morphemes for all words in the phrase.
        /// </summary>
        /// <param name="phrase"></param>
        /// <param name="maxMorphDistance"></param>
        /// <returns></returns>
        PhraseDecomposition DecomposePhrase(IEnumerable<string> phrase, int maxMorphDistance);

        /// <summary>
        /// Finds all possible word constructions (morpheme sequences) for the word.
        /// </summary>
        /// <param name="word"></param>
        /// <param name="maxMorphDistance">The level of similarity. If 0 then the exact morph is searched.</param>
        /// <returns></returns>
        WordDecomposition DecomposeWord(string word, int maxMorphDistance);

        /// <summary>
        /// Finds patterns matching the morpheme (lexeme or non-lexeme).
        /// </summary>
        /// <param name="morpheme"></param>
        /// <returns></returns>
        IEnumerable<Pattern> FindPatterns(Morpheme morpheme);

        /// <summary>
        /// Finds transference patterns which change the grammar character of a morpheme.
        /// </summary>
        /// <remarks>
        /// E.g. it changes stative to adjunctive (A>O).
        /// </remarks>
        /// <param name="morpheme"></param>
        /// <returns></returns>
        IEnumerable<Pattern> FindMonoTransferencePatterns(Morpheme morpheme);


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

        /// <summary>
        /// Returns adtree constructions matching the pattern signature.
        /// </summary>
        /// <param name="patternSignature"></param>
        /// <returns></returns>
        IEnumerable<IAdTree> FindAdTreeConstructions(string patternSignature);
    }
}
