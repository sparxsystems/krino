using Krino.Domain.ConstructiveAdpositionalGrammar.AttributesArrangement.Semantic;
using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AttributesArrangement.Structural
{
    /// <summary>
    /// Verb attributes.
    /// </summary>
    public class VerbAttributes : EnumGroupBase
    {
        public VerbAttributes(EnumGroupBase parent) : base(parent)
        {
            Modal = new EnumValue(this);
            Regular = new EnumValue(this);
            Irregular = new EnumValue(this);
            Unergative = new EnumValue(this);
            Unaccusative = new EnumValue(this);
            Form = new VerbFormAttributes(this);
            Valency = new VerbValencyAttributes(this);
            Sememe = new VerbSememes(this);
        }

        /// <summary>
        /// The verb is modal. e.g. may, can, will.
        /// </summary>
        public EnumValue Modal { get; }

        /// <summary>
        /// Regular verb.
        /// </summary>
        public EnumValue Regular { get; }

        /// <summary>
        /// Irregular verb.
        /// </summary>
        public EnumValue Irregular { get; }

        /// <summary>
        /// If stative does the verbant.
        /// </summary>
        public EnumValue Unergative { get; }

        /// <summary>
        /// Verbant happens to the actant (stative).
        /// </summary>
        public EnumValue Unaccusative { get; }

        /// <summary>
        /// Form of the verb.
        /// </summary>
        public VerbFormAttributes Form { get; }

        /// <summary>
        /// Valency of the verb.
        /// </summary>
        public VerbValencyAttributes Valency { get; }

        /// <summary>
        /// Semantic attributes.
        /// </summary>
        public VerbSememes Sememe { get; }
    }
}
