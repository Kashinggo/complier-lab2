/****************************************************/
/* File: scan.c                                     */
/* The scanner implementation for the TINY compiler */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/
#pragma warning(disable:4996)
#include "globals.h"
#include "util.h"
#include "scan.h"

/* states in scanner DFA */
typedef enum
{
	START, INASSIGN, INCOMMENT, INNUM, INID, DONE, BIG, SMALL, INUPDOX
}
StateType;

/* lexeme of identifier or reserved word */
char tokenString[MAXTOKENLEN + 1];

/* BUFLEN = length of the input buffer for
   source code lines */
#define BUFLEN 256

static char lineBuf[BUFLEN]; /* holds the current line */
static int linepos = 0; /* current position in LineBuf */
static int bufsize = 0; /* current size of buffer string */
static int EOF_flag = FALSE; /* corrects ungetNextChar behavior on EOF */

/* getNextChar fetches the next non-blank character
   from lineBuf, reading in a new line if lineBuf is
   exhausted */
static int getNextChar(void)
{
	if (!(linepos < bufsize))
	{
		lineno++;
		if (fgets(lineBuf, BUFLEN - 1, source))
		{
			if (EchoSource)
				fprintf(listing, "%d: %s", lineno, lineBuf);
			bufsize = strlen(lineBuf);
			linepos = 0;
			return lineBuf[linepos++];
		}
		else
		{
			EOF_flag = TRUE;
			return EOF;
		}
	}
	else return lineBuf[linepos++];
}

/* ungetNextChar backtracks one character
   in lineBuf */
static void ungetNextChar(void)
{
	if (!EOF_flag) linepos--;
}

/* lookup table of reserved words */
static struct
{
	char* str;
	TokenType tok;
} reservedWords[MAXRESERVED]
= { {"if",IF},{"then",THEN},{"else",ELSE},{"end",END},
   {"repeat",REPEAT},{"until",UNTIL},{"read",READ},{"write",WRITE},
   {"true",TRUE1},{"false",FALSE1}, {"or",OR},{"and",AND},
   {"not",NOT}, {"int",INT}, {"bool",BOOL},
   {"string",STRING}, {"do",DO}, {"while",WHILE} };

/* lookup an identifier to see if it is a reserved word */
/* uses linear search */
static TokenType reservedLookup(char* s)
{
	int i;
	for (i = 0; i < MAXRESERVED; i++)
		if (!strcmp(s, reservedWords[i].str))
			return reservedWords[i].tok;
	return ID;
}

/****************************************/
/* the primary function of the scanner  */
/****************************************/
/* function getToken returns the
 * next token in source file
 */
TokenType getToken(void)
{  /* index for storing into tokenString */
	int tokenStringIndex = 0;
	/* holds current token to be returned */
	TokenType currentToken;
	/* current state - always begins at START */
	StateType state = START;
	/* flag to indicate save to tokenString */
	int save;
	while (state != DONE)
	{
		int c = getNextChar();
		save = TRUE;
		switch (state)
		{
		case START:
			if (isdigit(c))
				state = INNUM;
			else if (isalpha(c))
				state = INID;
			else if (c == ':')
				state = INASSIGN;
			else if (c == '>')
				state = BIG;
			else if (c == '<')
				state = SMALL;
			else if ((c == ' ') || (c == '\t') || (c == '\n') || (c == '\r'))
				save = FALSE;
			else if (c == '{')
			{
				save = FALSE;
				state = INCOMMENT;
			}
			else if (c == '\'')
			{
				save = FALSE;
				state = INUPDOX;
			}
			else
			{
				state = DONE;
				switch (c)
				{
				case EOF:
					save = FALSE;
					currentToken = ENDFILE;
					break;
				case '=':
					currentToken = EQ;
					break;
				case '+':
					currentToken = PLUS;
					break;
				case '-':
					currentToken = MINUS;
					break;
				case ',':
					currentToken = COMMA;
					break;
				case '*':
					currentToken = TIMES;
					break;
				case '/':
					currentToken = OVER;
					break;
				case '(':
					currentToken = LPAREN;
					break;
				case ')':
					currentToken = RPAREN;
					break;
				case ';':
					currentToken = SEMI;
					break;
				default:
					currentToken = ERROR;
					break;
				}
			}
			break;
		case INCOMMENT://处理注释{}
			save = FALSE;
			if (c == EOF)//读到文件尾也没有右括号
			{
				state = DONE;
				currentToken = ERROR;
				strcpy(tokenString, "Missing \" } \" !");//出错信息写入tokenString
				tokenStringIndex += 15;//字符串偏移15个字符
			}
			else if (c == '}')
				state = START;
			break;
		case INUPDOX://处理单引号'
			if (c == '\'')//读到右单引号
			{
				save = FALSE;
				state = DONE;
				currentToken = STR;
			}
			else if (!(linepos < bufsize))//读到行末没有右单引号
			{
				save = FALSE;
				state = DONE;
				currentToken = ERROR;
				strcpy(tokenString, "Missing \" \' \" !");
				tokenStringIndex += 15;
			}
			break;
		case BIG://处理>
			state = DONE;
			if (c == '=')
				currentToken = ME;
			else
			{ /* backup in the input */
				ungetNextChar();
				save = FALSE;
				currentToken = MT;
			}
			break;
		case SMALL://处理<
			state = DONE;
			if (c == '=')
				currentToken = LE;
			else
			{ /* backup in the input */
				ungetNextChar();
				save = FALSE;
				currentToken = LT;
			}
			break;
		case INASSIGN:
			state = DONE;
			if (c == '=')
				currentToken = ASSIGN;
			else
			{ /* backup in the input */
				ungetNextChar();
				save = FALSE;
				currentToken = ERROR;
			}
			break;
		case INNUM:
			if (!isdigit(c))
			{ /* backup in the input */
				ungetNextChar();
				save = FALSE;
				state = DONE;
				currentToken = NUM;
			}
			break;
		case INID:
			if (!(isalpha(c)||isdigit(c)))//允许ID非首字母为数字
			{ /* backup in the input */
				ungetNextChar();
				save = FALSE;
				state = DONE;
				currentToken = ID;
			}
			break;
		case DONE:
		default: /* should never happen */
			fprintf(listing, "Scanner Bug: state= %d\n", state);
			state = DONE;
			currentToken = ERROR;
			break;
		}
		if ((save) && (tokenStringIndex <= MAXTOKENLEN))
			tokenString[tokenStringIndex++] = (char)c;
		if (state == DONE)
		{
			tokenString[tokenStringIndex] = '\0';
			if (currentToken == ID)
				currentToken = reservedLookup(tokenString);
		}
	}
	if (TraceScan) {
		fprintf(listing, "\t%d: ", lineno);
		printToken(currentToken, tokenString);
	}
	return currentToken;
} /* end getToken */


