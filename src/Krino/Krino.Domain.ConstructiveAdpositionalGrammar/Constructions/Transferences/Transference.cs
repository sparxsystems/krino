using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Transformations;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Transferences
{
    /// <summary>
    /// Utility to instantiate transferences.
    /// </summary>
    public static class Transference
    {
        /// <summary>
        /// Empty transference.
        /// </summary>
        public static ITransformation<IMorpheme> NothingToDo => Trans.NothingToDo<IMorpheme>();

        /// <summary>
        /// Derivation transference of attributes including grammar character. The morph is not changed.
        /// </summary>
        /// <param name="attributesTransformation"></param>
        /// <returns></returns>
        public static DerivationMorphemeTransference AttributesDerivation(ITransformation<BigInteger> attributesTransformation)
            => Derivation(Trans.NothingToDo<string>(), attributesTransformation);

        /// <summary>
        /// Derivation transference of the morph. Attributes are not changed.
        /// </summary>
        /// <param name="morphTransformation"></param>
        /// <returns></returns>
        public static DerivationMorphemeTransference MorphDerivation(ITransformation<string> morphTransformation)
            => Derivation(morphTransformation, Trans.NothingToDo<BigInteger>());

        /// <summary>
        /// Derivation transference.
        /// </summary>
        /// <param name="morphTransformation"></param>
        /// <param name="attributesTransformation"></param>
        /// <returns></returns>
        public static DerivationMorphemeTransference Derivation(ITransformation<string> morphTransformation, ITransformation<BigInteger> attributesTransformation)
            => new DerivationMorphemeTransference(morphTransformation, attributesTransformation);
    }
}
