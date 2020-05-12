using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    /// <summary>
    /// Declares the parser to seriaize and deseriaize adtrees.
    /// </summary>
    public interface IParser
    {
        /// <summary>
        /// Serializes the adtree into the text.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        string Serialize(IAdTree adTree);

        /// <summary>
        /// Deserializes the text into the adtree.
        /// </summary>
        /// <param name="text"></param>
        /// <param name="maxMorphDistance">The max level of similarity accepted for morphs.
        /// If 0 then the morph strict must exactly match with the lexeme in the dictionary.
        /// The distance represents the distance in the Levenshtein algorithm.
        /// </param>
        /// <returns></returns>
        IAdTree Deserialize(string text, int maxMorphDistance);
    }
}
