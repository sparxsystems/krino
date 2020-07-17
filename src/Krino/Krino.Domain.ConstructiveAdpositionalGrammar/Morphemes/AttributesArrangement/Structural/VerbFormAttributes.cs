using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural
{
    public class VerbFormAttributes : EnumGroupBase
    {
        public VerbFormAttributes(EnumGroupBase parent) : base(parent)
        {
            Infinitive = new EnumValue(this);
            PresentParticiple = new EnumValue(this);
            PastParticiple = new EnumValue(this);
            Gerund = new EnumValue(this);
        }

        /// <summary>
        /// Infinitive form of the verb.
        /// </summary>
        public EnumValue Infinitive { get; }

        /// <summary>
        /// A verb form which can be part of the continous verb construction, can act as an adjective or can be positioned after the verb.
        /// </summary>
        /// <remarks>
        /// After the verb example:
        /// We saw him swimming across the pond.
        /// She sat looking at the sea.
        /// </remarks>
        public EnumValue PresentParticiple { get; }

        /// <summary>
        /// A verb form which is used in forming perfect and passive tenses and sometimes as an adjective.
        /// (Third form of the verb.)
        /// </summary>
        public EnumValue PastParticiple { get; }

        /// <summary>
        /// A verb form which acts as a noun. In English ending with -ing.
        /// </summary>
        public EnumValue Gerund { get; }
    }
}
