using Krino.Vertical.Utils.Enums;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    [DebuggerDisplay("{DebuggerDisplay}")]
    public class Morpheme : IEquatable<Morpheme>
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

        public string Morph { get; private set; }

        public IAttributesModel AttributesModel { get; private set; }

        public BigInteger Attributes { get; private set; }

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

        public bool Equals(Morpheme other) => Morph == other.Morph && Attributes == other.Attributes;

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ Morph.GetHashCode();
            hash = (hash * 16777619) ^ Attributes.GetHashCode();

            return hash;
        }

        private string DebuggerDisplay => string.Join("", GrammarCharacter, ": ", Morph);
    }
}
