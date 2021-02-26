using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Semantic
{
    public class CauseConjunctionSememes : EnumGroupBase
    {
        public CauseConjunctionSememes(EnumGroupBase parent) : base(parent)
        {
            ConclusionPremise = new EnumValue(this);
        }

        /// <summary>
        /// Indicates order conclusion-conjunction-premise. E.g. conclusion because premise.
        /// </summary>
        public EnumValue ConclusionPremise { get; }
    }
}
