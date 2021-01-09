using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Transformations;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Transferences
{
    /// <summary>
    /// Utility to instantiate transferences.
    /// </summary>
    public static class Transference
    {
        /// <summary>
        /// Empty transference.
        /// </summary>
        public static ITransformation<Morpheme> NothingToDo => Trans.NothingToDo<Morpheme>();

        /// <summary>
        /// Derivation transference of attributes including grammar character. The morph is not changed.
        /// </summary>
        /// <param name="attributesTransformation"></param>
        /// <returns></returns>
        public static DerivationMorphemeTransference AttributesDerivation(IAttributesModel attributesModel, ITransformation<BigInteger> attributesTransformation)
            => Derivation(attributesModel, Trans.NothingToDo<string>(), attributesTransformation);

        /// <summary>
        /// Derivation transference of the morph. Attributes are not changed.
        /// </summary>
        /// <param name="morphTransformation"></param>
        /// <returns></returns>
        public static DerivationMorphemeTransference MorphDerivation(IAttributesModel attributesModel, ITransformation<string> morphTransformation)
            => Derivation(attributesModel, morphTransformation, Trans.NothingToDo<BigInteger>());

        /// <summary>
        /// Derivation transference.
        /// </summary>
        /// <param name="morphTransformation"></param>
        /// <param name="attributesTransformation"></param>
        /// <returns></returns>
        public static DerivationMorphemeTransference Derivation(IAttributesModel attributesModel, ITransformation<string> morphTransformation, ITransformation<BigInteger> attributesTransformation)
            => new DerivationMorphemeTransference(attributesModel, morphTransformation, attributesTransformation);
    }
}
