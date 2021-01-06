using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Transformations;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Transferences
{
    /// <summary>
    /// Transfers the morph and the grammar character.
    /// </summary>
    public class DerivationMorphemeTransference : ITransformation<Morpheme>
    {
        private ITransformation<string> myMorphTransformation;
        private ITransformation<BigInteger> myAttributeTransformation;


        public DerivationMorphemeTransference(ITransformation<string> morphTransformation, ITransformation<BigInteger> attributeTransformation)
        {
            myMorphTransformation = morphTransformation;
            myAttributeTransformation = attributeTransformation;
        }


        public Morpheme Transform(Morpheme value)
        {
            string transformedMorph = myMorphTransformation.Transform(value.Morph);
            BigInteger transformedAttributes = myAttributeTransformation.Transform(value.Attributes);
            Morpheme result = new Morpheme(transformedMorph, transformedAttributes);
            return result;
        }

        public bool Equals(ITransformation<Morpheme> other) =>
            other is DerivationMorphemeTransference otherTransformation &&
            myMorphTransformation.Equals(otherTransformation.myMorphTransformation);
    }
}
