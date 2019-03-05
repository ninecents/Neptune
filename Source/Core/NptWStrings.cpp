/*****************************************************************
|
|   Neptune - String Objects
|
| Copyright (c) 2002-2008, Axiomatic Systems, LLC.
| All rights reserved.
|
| Redistribution and use in source and binary forms, with or without
| modification, are permitted provided that the following conditions are met:
|     * Redistributions of source code must retain the above copyright
|       notice, this list of conditions and the following disclaimer.
|     * Redistributions in binary form must reproduce the above copyright
|       notice, this list of conditions and the following disclaimer in the
|       documentation and/or other materials provided with the distribution.
|     * Neither the name of Axiomatic Systems nor the
|       names of its contributors may be used to endorse or promote products
|       derived from this software without specific prior written permission.
|
| THIS SOFTWARE IS PROVIDED BY AXIOMATIC SYSTEMS ''AS IS'' AND ANY
| EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
| WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
| DISCLAIMED. IN NO EVENT SHALL AXIOMATIC SYSTEMS BE LIABLE FOR ANY
| DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
| (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
| LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
| ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
| (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
| SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|
 ****************************************************************/

/*----------------------------------------------------------------------
|   includes
+---------------------------------------------------------------------*/
#include "NptConfig.h"
#include "NptTypes.h"
#include "NptConstants.h"
#include "NptWStrings.h"
#include "NptResults.h"
#include "NptUtils.h"
#include "NptDebug.h"

/*----------------------------------------------------------------------
|   constants
+---------------------------------------------------------------------*/
#define NPT_WStringS_WHITESPACE_CHARS L"\r\n\t "

const unsigned int NPT_WString_FORMAT_BUFFER_DEFAULT_SIZE = 256;
const unsigned int NPT_WString_FORMAT_BUFFER_MAX_SIZE     = 0x80000; // 512k

/*----------------------------------------------------------------------
|   helpers
+---------------------------------------------------------------------*/
inline char NPT_Uppercase(wchar_t x) {
    return (x >= L'a' && x <= L'z') ? x&0xdf : x;
}

inline wchar_t NPT_Lowercase(wchar_t x) {
    return (x >= L'A' && x <= L'Z') ? x^32 : x;
}
               
/*----------------------------------------------------------------------
|   NPT_WString::EmptyString
+---------------------------------------------------------------------*/
wchar_t NPT_WString::EmptyString = '\0';

/*----------------------------------------------------------------------
|   NPT_WString::FromInteger
+---------------------------------------------------------------------*/
NPT_WString
NPT_WString::FromInteger(NPT_Int64 value)
{
    wchar_t str[32];
    wchar_t* c = &str[31];
    *c-- = '\0';

    // handle the sign
    bool negative = false;
    if (value < 0) {
        negative = true;
        value = -value;
    }

    // process the digits
    do {
        int digit = (int)(value%10);
        *c-- = '0'+digit;
        value /= 10;
    } while(value);

    if (negative) {
        *c = '-';
    } else {
        ++c;
    }

    return NPT_WString(c);
}

/*----------------------------------------------------------------------
|   NPT_WString::FromIntegerU
+---------------------------------------------------------------------*/
NPT_WString
NPT_WString::FromIntegerU(NPT_UInt64 value)
{
    wchar_t str[32];
    wchar_t* c = &str[31];
    *c = '\0';

    // process the digits
    do {
        int digit = (int)(value%10);
        *--c = '0'+digit;
        value /= 10;
    } while(value);

    return NPT_WString(c);
}

/*----------------------------------------------------------------------
|   NPT_WString::Format
+---------------------------------------------------------------------*/
NPT_WString
NPT_WString::Format(const wchar_t* format, ...)
{
    NPT_WString result;
    NPT_Size   buffer_size = NPT_WString_FORMAT_BUFFER_DEFAULT_SIZE; // default value
    
    va_list  args;

    for(;;) {
        /* try to format (it might not fit) */
        result.Reserve(buffer_size);
        wchar_t* buffer = result.UseChars();
        va_start(args, format);
        int f_result = NPT_FormatWStringVN(buffer, buffer_size, format, args);
        va_end(args);
        if (f_result >= (int)(buffer_size)) f_result = -1;
        if (f_result >= 0) {
            result.SetLength(f_result);
            break;
        }
        
        /* the buffer was too small, try something bigger         */
        /* (we don't trust the return value of NPT_FormatStringVN */
        /* for the actual size needed)                            */
        buffer_size *= 2;
        if (buffer_size > NPT_WString_FORMAT_BUFFER_MAX_SIZE) break;
    }
    
    return result;
}

/*----------------------------------------------------------------------
|   NPT_WString::NPT_WString
+---------------------------------------------------------------------*/
NPT_WString::NPT_WString(const wchar_t* str)
{
    if (str == NULL) {
        m_Chars = NULL;
    } else {
        m_Chars = Buffer::Create(str);
    }
}

/*----------------------------------------------------------------------
|   NPT_WString::NPT_WString
+---------------------------------------------------------------------*/
NPT_WString::NPT_WString(const wchar_t* str, NPT_Size length)
{
    if (str == NULL || length == 0) {
        m_Chars = NULL;
    } else {
        for (unsigned int i=0; i<length-1; i++) {
            if (str[i] == '\0') {
                if (i == 0) {
                    m_Chars = NULL;
                    return;
                }
                length = i;
                break;
            }
        }
        m_Chars = Buffer::Create(str, length);
    }
}

/*----------------------------------------------------------------------
|   NPT_WString::NPT_WString
+---------------------------------------------------------------------*/
NPT_WString::NPT_WString(const NPT_WString& str)
{
    if (str.GetLength() == 0) {
        m_Chars = NULL;
    } else {
        m_Chars = Buffer::Create(str.GetChars(), str.GetLength());
    }
}

/*----------------------------------------------------------------------
|   NPT_WString::NPT_WString
+---------------------------------------------------------------------*/
NPT_WString::NPT_WString(wchar_t c, NPT_Cardinal repeat)
{
    if (repeat != 0) {
        m_Chars = Buffer::Create(c, repeat);
    } else {
        m_Chars = NULL;
    }
}

/*----------------------------------------------------------------------
|   NPT_WString::SetLength
+---------------------------------------------------------------------*/
NPT_Result
NPT_WString::SetLength(NPT_Size length, bool pad)
{
    // special case for 0
    if (length == 0) {
        Reset();
        return NPT_SUCCESS;
    }
    
    // reserve the space
    Reserve(length);

    // pad with spaces if necessary
    wchar_t* chars = UseChars();
    if (pad) {
        unsigned int current_length = GetLength();
        if (length > current_length) {
            unsigned int pad_length = length-current_length;
            NPT_SetMemory(chars+current_length, ' ', pad_length);
        }
    }
    
    // update the length and terminate the buffer
    GetBuffer()->SetLength(length);
    chars[length] = '\0';
    
    return NPT_SUCCESS;
}

/*----------------------------------------------------------------------
|   NPT_WString::PrepareToWrite
+---------------------------------------------------------------------*/
inline wchar_t*
NPT_WString::PrepareToWrite(NPT_Size length)
{
    NPT_ASSERT(length != 0);
    if (m_Chars == NULL || GetBuffer()->GetAllocated() < length) {
        // the buffer is too small, we need to allocate a new one.
        NPT_Size needed = length;
        if (m_Chars != NULL) {
            NPT_Size grow = GetBuffer()->GetAllocated()*2;
            if (grow > length) needed = grow;
            delete GetBuffer();
        }
        m_Chars = Buffer::Create(needed);
    }    
    GetBuffer()->SetLength(length);
    return m_Chars;
}

/*----------------------------------------------------------------------
|   NPT_WString::Reserve
+---------------------------------------------------------------------*/
void
NPT_WString::Reserve(NPT_Size allocate)
{
    if (m_Chars == NULL || GetBuffer()->GetAllocated() < allocate) {
        // the buffer is too small, we need to allocate a new one.
        NPT_Size needed = allocate;
        if (m_Chars != NULL) {
            NPT_Size grow = GetBuffer()->GetAllocated()*2;
            if (grow > allocate) needed = grow;
        }
        NPT_Size length = GetLength();
        wchar_t* copy = Buffer::Create(needed, length);
        if (m_Chars != NULL) {
            CopyString(copy, m_Chars);
            delete GetBuffer();
        } else {
            copy[0] = '\0';
        }
        m_Chars = copy;
    }
}

/*----------------------------------------------------------------------
|   NPT_WString::Assign
+---------------------------------------------------------------------*/
void
NPT_WString::Assign(const wchar_t* str, NPT_Size length)
{
    if (str == NULL || length == 0) {
        Reset();
    } else {
        for (unsigned int i=0; i<length-1; i++) {
            if (str[i] == '\0') {
                if (i == 0) {
                    Reset();
                    return;
                } else {
                    length = i;
                    break;
                }
            }
        }
        PrepareToWrite(length);
        CopyBuffer(m_Chars, str, length);
        m_Chars[length] = '\0';
    }
}

/*----------------------------------------------------------------------
|   NPT_WString::operator=
+---------------------------------------------------------------------*/
NPT_WString&
NPT_WString::operator=(const wchar_t* str)
{
    if (str == NULL) {
        Reset();
    } else {
        NPT_Size length = StringLength(str);
        if (length == 0) {
            Reset();
        } else {
            CopyString(PrepareToWrite(length), str);
        }
    }

    return *this;
}

/*----------------------------------------------------------------------
|   NPT_WString::operator=
+---------------------------------------------------------------------*/
NPT_WString&
NPT_WString::operator=(const NPT_WString& str)
{
    // do nothing if we're assigning to ourselves
    if (this != &str) {
        Assign(str.GetChars(), str.GetLength());
    }
    return *this;
}

/*----------------------------------------------------------------------
|   NPT_WString::GetHash32
+---------------------------------------------------------------------*/
NPT_UInt32 
NPT_WString::GetHash32() const
{
    return NPT_Fnv1aHashStr32(GetChars());
}

/*----------------------------------------------------------------------
|   NPT_WString::GetHash64
+---------------------------------------------------------------------*/
NPT_UInt64
NPT_WString::GetHash64() const
{
    return NPT_Fnv1aHashStr64(GetChars());
}

/*----------------------------------------------------------------------
|   NPT_WString::Append
+---------------------------------------------------------------------*/
void
NPT_WString::Append(const wchar_t* str, NPT_Size length)
{
    // shortcut
    if (str == NULL || length == 0) return;

    // compute the new length
    NPT_Size old_length = GetLength();
    NPT_Size new_length = old_length + length;

    // allocate enough space
    Reserve(new_length);
    
    // append the new string at the end of the current one
    CopyBuffer(m_Chars+old_length, str, length);
    m_Chars[new_length] = '\0';

    // update the length
    GetBuffer()->SetLength(new_length);
}

/*----------------------------------------------------------------------
|   NPT_WString::Compare
+---------------------------------------------------------------------*/
int 
NPT_WString::Compare(const wchar_t *s, bool ignore_case) const
{
    return NPT_WString::Compare(GetChars(), s, ignore_case);
}

/*----------------------------------------------------------------------
|   NPT_WString::Compare
+---------------------------------------------------------------------*/
int 
NPT_WString::Compare(const wchar_t *s1, const wchar_t *s2, bool ignore_case)
{
    const wchar_t *r1 = s1;
    const wchar_t *r2 = s2;

    if (ignore_case) {
        while (NPT_Uppercase(*r1) == NPT_Uppercase(*r2)) {
            if (*r1++ == '\0') {
                return 0;
            } 
            r2++;
        }
        return NPT_Uppercase(*r1) - NPT_Uppercase(*r2);
    } else {
        while (*r1 == *r2) {
            if (*r1++ == '\0') {
                return 0;
            } 
            r2++;
        }
        return (*r1 - *r2);
    }
}

/*----------------------------------------------------------------------
|   NPT_WString::CompareN
+---------------------------------------------------------------------*/
int 
NPT_WString::CompareN(const wchar_t *s, NPT_Size count, bool ignore_case) const
{
    return NPT_WString::CompareN(GetChars(), s, count, ignore_case);
}

/*----------------------------------------------------------------------
|   NPT_WString::CompareN
+---------------------------------------------------------------------*/
int 
NPT_WString::CompareN(const wchar_t* s1, const wchar_t *s2, NPT_Size count, bool ignore_case)
{
    const wchar_t* me = s1;

    if (ignore_case) {
        for (unsigned int i=0; i<count; i++) {
            if (NPT_Uppercase(me[i]) != NPT_Uppercase(s2[i])) {
                return NPT_Uppercase(me[i]) - NPT_Uppercase(s2[i]);
            }
        }
        return 0;
    } else {
        for (unsigned int i=0; i<count; i++) {
            if (me[i] != s2[i]) {
                return (me[i] - s2[i]);
            }
        }
        return 0;
    }
}

/*----------------------------------------------------------------------
|   NPT_WString::Split
+---------------------------------------------------------------------*/
NPT_List<NPT_WString> 
NPT_WString::Split(const wchar_t* separator) const
{
    NPT_List<NPT_WString> result;
    NPT_Size             separator_length = NPT_WStringLength(separator);
    
    // sepcial case for empty separators
    if (separator_length == 0) {
        result.Add(*this);
        return result;
    }
    
    int current = 0;  
    int next;  
    do {
        next = Find(separator, current);
        unsigned int end = (next>=0?(unsigned int)next:GetLength());
        result.Add(SubString(current, end-current));
        current = next+separator_length;
    } while (next >= 0);
    
    return result;
}

/*----------------------------------------------------------------------
|   NPT_WString::Join
+---------------------------------------------------------------------*/
NPT_WString
NPT_WString::Join(NPT_List<NPT_WString>& args, const wchar_t* separator)
{
    NPT_WString output;
    NPT_List<NPT_WString>::Iterator arg = args.GetFirstItem();
    while (arg) {
        output += *arg;
        if (++arg) output += separator;
    }
    
    return output;
}

/*----------------------------------------------------------------------
|   NPT_WString::SubString
+---------------------------------------------------------------------*/
NPT_WString
NPT_WString::SubString(NPT_Ordinal first, NPT_Size length) const
{
    if (first >= GetLength()) {
        first = GetLength();
        length = 0;
    } else if (first+length >= GetLength()) {
        length = GetLength()-first;
    }
    return NPT_WString(GetChars()+first, length);
}

/*----------------------------------------------------------------------
|   NPT_WStringStartsWith
|
|    returns:
|   1 if str starts with sub,
|   0 if str is large enough but does not start with sub
|     -1 if str is too short to start with sub
+---------------------------------------------------------------------*/
static inline int
NPT_WStringStartsWith(const wchar_t* str, const wchar_t* sub, bool ignore_case)
{
    if (ignore_case) {
        while (NPT_Uppercase(*str) == NPT_Uppercase(*sub)) {
            if (*str++ == '\0') {
                return 1;
            }
            sub++;
        }
    } else {
        while (*str == *sub) {
            if (*str++ == '\0') {
                return 1;
            }
            sub++;
        }
    }
    return (*sub == '\0') ? 1 : (*str == '\0' ? -1 : 0);
}

/*----------------------------------------------------------------------
|   NPT_WString::StartsWith
+---------------------------------------------------------------------*/
bool 
NPT_WString::StartsWith(const wchar_t *s, bool ignore_case) const
{
    if (s == NULL) return false;
    return NPT_WStringStartsWith(GetChars(), s, ignore_case) == 1;
}

/*----------------------------------------------------------------------
|   NPT_WString::EndsWith
+---------------------------------------------------------------------*/
bool 
NPT_WString::EndsWith(const wchar_t *s, bool ignore_case) const
{
    if (s == NULL) return false;
    NPT_Size str_length = NPT_WStringLength(s);
    if (str_length > GetLength()) return false;
    return NPT_WStringStartsWith(GetChars()+GetLength()-str_length, s, ignore_case) == 1;
}

/*----------------------------------------------------------------------
|   NPT_WString::Find
+---------------------------------------------------------------------*/
int
NPT_WString::Find(const wchar_t* str, NPT_Ordinal start, bool ignore_case) const
{
    // check args
    if (str == NULL || start >= GetLength()) return -1;

    // skip to start position
    const wchar_t* src = m_Chars + start;

    // look for a substring
    while (*src) {
        int cmp = NPT_WStringStartsWith(src, str, ignore_case);
        switch (cmp) {
            case -1:
                // ref is too short, abort
                return -1;
            case 1:
                // match
                return (int)(src-m_Chars);
        }
        src++;
    }

    return -1;
}

/*----------------------------------------------------------------------
|   NPT_WString::Find
+---------------------------------------------------------------------*/
int
NPT_WString::Find(wchar_t c, NPT_Ordinal start, bool ignore_case) const
{
    // check args
    if (start >= GetLength()) return -1;

    // skip to start position
    const wchar_t* src = m_Chars + start;

    // look for the character
    if (ignore_case) {
        while (*src) {
            if (NPT_Uppercase(*src) == NPT_Uppercase(c)) {
                return (int)(src-m_Chars);
            }
            src++;
        }
    } else {
        while (*src) {
            if (*src == c) return (int)(src-m_Chars);
            src++;
        }
    }

    return -1;
}

/*----------------------------------------------------------------------
|   NPT_WString::ReverseFind
+---------------------------------------------------------------------*/
int
NPT_WString::ReverseFind(const wchar_t* str, NPT_Ordinal start, bool ignore_case) const
{
    // check args
    if (str == NULL || *str == '\0') return -1;

    // look for a substring
    NPT_Size my_length = GetLength();
    NPT_Size str_length = NPT_WStringLength(str);
    int i=my_length-start-str_length;
    const wchar_t* src = GetChars();
    if (i<0) return -1;
    for (;i>=0; i--) {
        int cmp = NPT_WStringStartsWith(src+i, str, ignore_case);
        if (cmp == 1) {
            // match
            return i;
        }
    }

    return -1;
}

/*----------------------------------------------------------------------
|   NPT_WString::ReverseFind
+---------------------------------------------------------------------*/
int
NPT_WString::ReverseFind(wchar_t c, NPT_Ordinal start, bool ignore_case) const
{
    // check args
    NPT_Size length = GetLength();
    int i = length-start-1;
    if (i < 0) return -1;

    // look for the character
    const wchar_t* src = GetChars();
    if (ignore_case) {
        for (;i>=0;i--) {
            if (NPT_Uppercase(src[i]) == NPT_Uppercase(c)) {
                return i;
            }
        }
    } else {
        for (;i>=0;i--) {
            if (src[i] == c) return i;
        }
    }

    return -1;
}

/*----------------------------------------------------------------------
|   NPT_WString::MakeLowercase
+---------------------------------------------------------------------*/
void
NPT_WString::MakeLowercase()
{
    // the source is the current buffer
    const wchar_t* src = GetChars();

    // convert all the characters of the existing buffer
    wchar_t* dst = const_cast<wchar_t*>(src);
    while (*dst != '\0') {
        *dst = NPT_Lowercase(*dst);
        dst++;
    }
}

/*----------------------------------------------------------------------
|   NPT_WString::MakeUppercase
+---------------------------------------------------------------------*/
void
NPT_WString::MakeUppercase() 
{
    // the source is the current buffer
    const wchar_t* src = GetChars();

    // convert all the characters of the existing buffer
    wchar_t* dst = const_cast<wchar_t*>(src);
    while (*dst != '\0') {
        *dst = NPT_Uppercase(*dst);
        dst++;
    }
}

/*----------------------------------------------------------------------
|   NPT_WString::ToLowercase
+---------------------------------------------------------------------*/
NPT_WString
NPT_WString::ToLowercase() const
{
    NPT_WString result(*this);
    result.MakeLowercase();
    return result;
}

/*----------------------------------------------------------------------
|   NPT_WString::ToUppercase
+---------------------------------------------------------------------*/
NPT_WString
NPT_WString::ToUppercase() const
{
    NPT_WString result(*this);
    result.MakeUppercase();
    return result;
}

/*----------------------------------------------------------------------
|   NPT_WString::Replace
+---------------------------------------------------------------------*/
const NPT_WString&
NPT_WString::Replace(wchar_t a, wchar_t b) 
{
    // check args
    if (m_Chars == NULL || a == '\0' || b == '\0') return *this;

    // we are going to modify the characters
    wchar_t* src = m_Chars;

    // process the buffer in place
    while (*src) {
        if (*src == a) *src = b;
        src++;
    }
    return *this;
}

/*----------------------------------------------------------------------
|   NPT_WString::Replace
+---------------------------------------------------------------------*/
const NPT_WString&
NPT_WString::Replace(wchar_t a, const wchar_t* str) 
{
    // check args
    if (m_Chars == NULL || a == '\0' || str == NULL || str[0] == '\0') return *this;

    // optimization
    if (NPT_WStringLength(str) == 1) return Replace(a, str[0]);

    // we are going to create a new string
    NPT_WString dst;
    wchar_t* src = m_Chars;

    // reserve at least as much as input
    dst.Reserve(GetLength());

    // process the buffer
    while (*src) {
        if (*src == a) {
            dst += str;
        } else {
            dst += *src;
        }
        src++;
    }

    Assign(dst.GetChars(), dst.GetLength());
    return *this;
}

/*----------------------------------------------------------------------
|   NPT_WString::Replace
+---------------------------------------------------------------------*/
const NPT_WString&
NPT_WString::Replace(const wchar_t* before, const wchar_t* after)
{
    NPT_Size size_before = NPT_WStringLength(before);
    NPT_Size size_after  = NPT_WStringLength(after);
    int      index       = Find(before);
    while (index != NPT_WString_SEARCH_FAILED) {
        Erase(index, size_before);
        Insert(after, index);
        index = Find(before, index+size_after);
    }
    return *this;
}

/*----------------------------------------------------------------------
|   NPT_WString::Insert
+---------------------------------------------------------------------*/
const NPT_WString&
NPT_WString::Insert(const wchar_t* str, NPT_Ordinal where)
{
    // check args
    if (str == NULL || where > GetLength()) return *this;

    // measure the string to insert
    NPT_Size str_length = StringLength(str);
    if (str_length == 0) return *this;

    // compute the size of the new string
    NPT_Size old_length = GetLength();
    NPT_Size new_length = str_length + GetLength();

    // prepare to write the new string
    wchar_t* src = m_Chars;
    wchar_t* nst = Buffer::Create(new_length, new_length);
    wchar_t* dst = nst;

    // copy the beginning of the old string
    if (where > 0) {
        CopyBuffer(dst, src, where);
        src += where;
        dst += where;
    }

    // copy the inserted string
    CopyString(dst, str);
    dst += str_length;

    // copy the end of the old string
    if (old_length > where) {
        CopyString(dst, src);
    }

    // use the new string
    if (m_Chars) delete GetBuffer();
    m_Chars = nst;
    return *this;
}

/*----------------------------------------------------------------------
|   NPT_WString::Erase
+---------------------------------------------------------------------*/
const NPT_WString&
NPT_WString::Erase(NPT_Ordinal start, NPT_Cardinal count /* = 1 */)
{
    // check bounds
    NPT_Size length = GetLength();
    if (start+count > length) {
        if (start >= length) return *this;
        count = length-start;
    }
    if (count == 0) return *this;

    CopyString(m_Chars+start, m_Chars+start+count);
    GetBuffer()->SetLength(length-count);
    return *this;
}

/*----------------------------------------------------------------------
|    NPT_WString::ToInteger
+---------------------------------------------------------------------*/
NPT_Result 
NPT_WString::ToInteger(int& value, bool relaxed) const
{
    return NPT_ParseInteger(GetChars(), value, relaxed);
}

/*----------------------------------------------------------------------
|    NPT_WString::ToInteger
+---------------------------------------------------------------------*/
NPT_Result 
NPT_WString::ToInteger(unsigned int& value, bool relaxed) const
{
    return NPT_ParseInteger(GetChars(), value, relaxed);
}

/*----------------------------------------------------------------------
|    NPT_WString::ToInteger
+---------------------------------------------------------------------*/
NPT_Result 
NPT_WString::ToInteger(long& value, bool relaxed) const
{
    return NPT_ParseInteger(GetChars(), value, relaxed);
}

/*----------------------------------------------------------------------
|    NPT_WString::ToInteger
+---------------------------------------------------------------------*/
NPT_Result 
NPT_WString::ToInteger(unsigned long& value, bool relaxed) const
{
    return NPT_ParseInteger(GetChars(), value, relaxed);
}

/*----------------------------------------------------------------------
|    NPT_WString::ToInteger32
+---------------------------------------------------------------------*/
NPT_Result 
NPT_WString::ToInteger32(NPT_Int32& value, bool relaxed) const
{
    return NPT_ParseInteger32(GetChars(), value, relaxed);
}

/*----------------------------------------------------------------------
|    NPT_WString::ToInteger32
+---------------------------------------------------------------------*/
NPT_Result 
NPT_WString::ToInteger32(NPT_UInt32& value, bool relaxed) const
{
    return NPT_ParseInteger32(GetChars(), value, relaxed);
}

/*----------------------------------------------------------------------
|    NPT_WString::ToInteger64
+---------------------------------------------------------------------*/
NPT_Result 
NPT_WString::ToInteger64(NPT_Int64& value, bool relaxed) const
{
    return NPT_ParseInteger64(GetChars(), value, relaxed);
}

/*----------------------------------------------------------------------
|    NPT_WString::ToInteger64
+---------------------------------------------------------------------*/
NPT_Result 
NPT_WString::ToInteger64(NPT_UInt64& value, bool relaxed) const
{
    return NPT_ParseInteger64(GetChars(), value, relaxed);
}

/*----------------------------------------------------------------------
|    NPT_WString::ToFloat
+---------------------------------------------------------------------*/
NPT_Result 
NPT_WString::ToFloat(float& value, bool relaxed) const
{
    return NPT_ParseFloat(GetChars(), value, relaxed);
}

/*----------------------------------------------------------------------
|   NPT_WString::TrimLeft
+---------------------------------------------------------------------*/
const NPT_WString& 
NPT_WString::TrimLeft()
{
    return TrimLeft(NPT_WStringS_WHITESPACE_CHARS);
}

/*----------------------------------------------------------------------
|   NPT_WString::TrimLeft
+---------------------------------------------------------------------*/
const NPT_WString& 
NPT_WString::TrimLeft(wchar_t c)
{
    wchar_t s[2] = {c, 0};
    return TrimLeft((const wchar_t*)s);
}

/*----------------------------------------------------------------------
|   NPT_WString::TrimLeft
+---------------------------------------------------------------------*/
const NPT_WString& 
NPT_WString::TrimLeft(const wchar_t* chars)
{
    if (m_Chars == NULL) return *this;
    const wchar_t* s = m_Chars;
    while (wchar_t c = *s) {
        const wchar_t* x = chars;
        while (*x) {
            if (*x == c) break;
            x++;
        }
        if (*x == 0) break; // not found
        s++;
    }
    if (s == m_Chars) {
        // nothing was trimmed
        return *this;
    }

    // shift chars to the left
    wchar_t* d = m_Chars;
    GetBuffer()->SetLength(GetLength()-(NPT_Size)(s-d));
    while ((*d++ = *s++)) {};
    return *this;
}

/*----------------------------------------------------------------------
|   NPT_WString::TrimRight
+---------------------------------------------------------------------*/
const NPT_WString& 
NPT_WString::TrimRight()
{
    return TrimRight(NPT_WStringS_WHITESPACE_CHARS);
}

/*----------------------------------------------------------------------
|   NPT_WString::TrimRight
+---------------------------------------------------------------------*/
const NPT_WString& 
NPT_WString::TrimRight(wchar_t c)
{
    wchar_t s[2] = {c, 0};
    return TrimRight((const wchar_t*)s);
}

/*----------------------------------------------------------------------
|   NPT_WString::TrimRight
+---------------------------------------------------------------------*/
const NPT_WString& 
NPT_WString::TrimRight(const wchar_t* chars)
{
    if (m_Chars == NULL || m_Chars[0] == '\0') return *this;
    wchar_t* tail = m_Chars+GetLength()-1;
    wchar_t* s = tail;
    while (s != m_Chars-1) {
        const wchar_t* x = chars;
        while (*x) {
            if (*x == *s) {
                *s = '\0';
                break;
            }
            x++;
        }
        if (*x == 0) break; // not found
        s--;
    }
    if (s == tail) {
        // nothing was trimmed
        return *this;
    }
    GetBuffer()->SetLength(1+(int)(s-m_Chars));
    return *this;
}

/*----------------------------------------------------------------------
|   NPT_WString::Trim
+---------------------------------------------------------------------*/
const NPT_WString& 
NPT_WString::Trim()
{
    TrimLeft();
    return TrimRight();
}

/*----------------------------------------------------------------------
|   NPT_WString::Trim
+---------------------------------------------------------------------*/
const NPT_WString& 
NPT_WString::Trim(wchar_t c)
{
    wchar_t s[2] = {c, 0};
    TrimLeft((const wchar_t*)s);
    return TrimRight((const wchar_t*)s);
}

/*----------------------------------------------------------------------
|   NPT_WString::Trim
+---------------------------------------------------------------------*/
const NPT_WString& 
NPT_WString::Trim(const wchar_t* chars)
{
    TrimLeft(chars);
    return TrimRight(chars);
}

/*----------------------------------------------------------------------
|   NPT_WString::operator+(const NPT_WString&, const wchar_t*)
+---------------------------------------------------------------------*/
NPT_WString 
operator+(const NPT_WString& s1, const wchar_t* s2)
{
    // shortcut
    if (s2 == NULL) return NPT_WString(s1);

    // measure strings
    NPT_Size s1_length = s1.GetLength();
    NPT_Size s2_length = NPT_WString::StringLength(s2);

    // allocate space for the new string
    NPT_WString result;
    wchar_t* start = result.PrepareToWrite(s1_length+s2_length);

    // concatenate the two strings into the result
    NPT_WString::CopyBuffer(start, s1, s1_length);
    NPT_WString::CopyString(start+s1_length, s2);
    
    return result;
}

/*----------------------------------------------------------------------
|   NPT_WString::operator+(const NPT_WString& , const wchar_t*)
+---------------------------------------------------------------------*/
NPT_WString 
operator+(const wchar_t* s1, const NPT_WString& s2)
{
    // shortcut
    if (s1 == NULL) return NPT_WString(s2);

    // measure strings
    NPT_Size s1_length = NPT_WString::StringLength(s1);
    NPT_Size s2_length = s2.GetLength();

    // allocate space for the new string
    NPT_WString result;
    wchar_t* start = result.PrepareToWrite(s1_length+s2_length);

    // concatenate the two strings into the result
    NPT_WString::CopyBuffer(start, s1, s1_length);
    NPT_WString::CopyString(start+s1_length, s2.GetChars());
    
    return result;
}

/*----------------------------------------------------------------------
|   NPT_WString::operator+(const NPT_WString& , wchar_t)
+---------------------------------------------------------------------*/
NPT_WString 
operator+(const NPT_WString& s1, wchar_t c)
{
    // allocate space for the new string
    NPT_WString result;
    result.Reserve(s1.GetLength()+1);
    
    // append
    result = s1;
    result += c;

    return result;
}

