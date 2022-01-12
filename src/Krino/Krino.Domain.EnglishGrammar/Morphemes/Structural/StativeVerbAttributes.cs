using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Structural
{
    public class StativeVerbAttributes : EnumGroupBase
    {
        public StativeVerbAttributes(EnumGroupBase parent) : base(parent)
        {
            Linking = new EnumValue(this);
        }

        /// <summary>
        /// Linking verb
        /// </summary>
        /// <remarks>
        /// A linking verb tells us what the subject is, not what the subject is doing.
        /// </remarks>
        public EnumValue Linking { get; }
    }
}
