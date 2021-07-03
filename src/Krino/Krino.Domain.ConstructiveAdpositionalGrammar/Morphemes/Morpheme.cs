using Krino.Vertical.Utils.Enums;
using System.Collections.Generic;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    [DebuggerDisplay("{DebuggerDisplay}")]
    public class Morpheme
    {
        /// <summary>
        /// Indicates there is no morpheme.
        /// </summary>
        /// <remarks>
        /// This is used by zero-marked adpositions.
        /// </remarks>
        public static Morpheme Epsilon(IAttributesModel attributesModel) => new Morpheme(attributesModel, "", attributesModel.Epsilon);

        public Morpheme (IAttributesModel attributesModel, string morph, BigInteger attributes)
        {
            Morph = morph;
            Attributes = attributes;
            AttributesModel = attributesModel;
        }

        /// <summary>
        /// Creates the copy of the morpheme.
        /// </summary>
        /// <param name="morpheme"></param>
        public Morpheme(Morpheme morpheme)
            : this(morpheme.AttributesModel, morpheme.Morph, morpheme.Attributes)
        {
        }

        public IAttributesModel AttributesModel { get; private set; }

        public string Morph { get; set; }

        public BigInteger Attributes { get; set; }

        public IEnumerable<EnumBase> AttributeItems => AttributesModel.FindParticularAttributes(Attributes);

        public GrammarCharacter GrammarCharacter => AttributesModel.GetGrammarCharacter(Attributes);

        /// <summary>
        /// Returns true if the morpheme is a lexeme.
        /// </summary>
        /// <remarks>
        /// It does not have to be lexeme nor non-lexeme.
        /// </remarks>
        public bool IsLexeme => AttributesModel.IsLexeme(Attributes);

        /// <summary>
        /// Returns true if the morpheme is a non-lexeme.
        /// </summary>
        /// <remarks>
        /// It does not have to be lexeme nor non-lexeme.
        /// </remarks>
        public bool IsNonLexeme => AttributesModel.IsNonLexeme(Attributes);

        public bool IsPrefix => AttributesModel.IsPrefix(Attributes);

        public bool IsSuffix => AttributesModel.IsSuffix(Attributes);


        private string DebuggerDisplay => string.Join("", GrammarCharacter, ": ", Morph);
    }
}
