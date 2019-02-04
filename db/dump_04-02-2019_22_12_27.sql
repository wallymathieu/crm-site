--
-- PostgreSQL database dump
--

-- Dumped from database version 11.1 (Debian 11.1-1.pgdg90+1)
-- Dumped by pg_dump version 11.1 (Debian 11.1-1.pgdg90+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: public; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA public;


ALTER SCHEMA public OWNER TO postgres;

--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA public IS 'standard public schema';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: Activities; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Activities" (
    id bigint NOT NULL,
    description text NOT NULL,
    at timestamp with time zone NOT NULL,
    "contactId" bigint,
    "projectId" bigint
);


ALTER TABLE public."Activities" OWNER TO postgres;

--
-- Name: Activities_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public."Activities_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."Activities_id_seq" OWNER TO postgres;

--
-- Name: Activities_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public."Activities_id_seq" OWNED BY public."Activities".id;


--
-- Name: Comments; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Comments" (
    id bigint NOT NULL,
    comment text NOT NULL,
    at timestamp with time zone DEFAULT now() NOT NULL,
    "contactId" bigint,
    "projectId" bigint
);


ALTER TABLE public."Comments" OWNER TO postgres;

--
-- Name: Comments_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public."Comments_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."Comments_id_seq" OWNER TO postgres;

--
-- Name: Comments_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public."Comments_id_seq" OWNED BY public."Comments".id;


--
-- Name: ContactAssociations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."ContactAssociations" (
    "fromContactId" bigint NOT NULL,
    "toContactId" bigint NOT NULL,
    type text NOT NULL
);


ALTER TABLE public."ContactAssociations" OWNER TO postgres;

--
-- Name: ContactTags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."ContactTags" (
    "contactId" bigint NOT NULL,
    "tagName" text NOT NULL
);


ALTER TABLE public."ContactTags" OWNER TO postgres;

--
-- Name: Contacts; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Contacts" (
    id bigint NOT NULL,
    name text NOT NULL,
    phone text NOT NULL,
    email text NOT NULL
);


ALTER TABLE public."Contacts" OWNER TO postgres;

--
-- Name: Contacts_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public."Contacts_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."Contacts_id_seq" OWNER TO postgres;

--
-- Name: Contacts_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public."Contacts_id_seq" OWNED BY public."Contacts".id;


--
-- Name: ProjectTags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."ProjectTags" (
    "projectId" bigint NOT NULL,
    "tagName" text NOT NULL
);


ALTER TABLE public."ProjectTags" OWNER TO postgres;

--
-- Name: Projects; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Projects" (
    id bigint NOT NULL,
    description text NOT NULL,
    at timestamp with time zone DEFAULT now() NOT NULL,
    due timestamp with time zone NOT NULL
);


ALTER TABLE public."Projects" OWNER TO postgres;

--
-- Name: Projects_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public."Projects_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."Projects_id_seq" OWNER TO postgres;

--
-- Name: Projects_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public."Projects_id_seq" OWNED BY public."Projects".id;


--
-- Name: Tags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Tags" (
    name text NOT NULL
);


ALTER TABLE public."Tags" OWNER TO postgres;

--
-- Name: Activities id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Activities" ALTER COLUMN id SET DEFAULT nextval('public."Activities_id_seq"'::regclass);


--
-- Name: Comments id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Comments" ALTER COLUMN id SET DEFAULT nextval('public."Comments_id_seq"'::regclass);


--
-- Name: Contacts id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Contacts" ALTER COLUMN id SET DEFAULT nextval('public."Contacts_id_seq"'::regclass);


--
-- Name: Projects id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Projects" ALTER COLUMN id SET DEFAULT nextval('public."Projects_id_seq"'::regclass);


--
-- Data for Name: Activities; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Activities" (id, description, at, "contactId", "projectId") FROM stdin;
\.


--
-- Data for Name: Comments; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Comments" (id, comment, at, "contactId", "projectId") FROM stdin;
1	Called but got no answer	2019-02-04 20:33:46.536245+00	1	1
2	Called but got no answer	2019-02-04 20:33:51.594757+00	2	1
\.


--
-- Data for Name: ContactAssociations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."ContactAssociations" ("fromContactId", "toContactId", type) FROM stdin;
1	2	hired_by
\.


--
-- Data for Name: ContactTags; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."ContactTags" ("contactId", "tagName") FROM stdin;
1	lead
2	lead
\.


--
-- Data for Name: Contacts; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Contacts" (id, name, phone, email) FROM stdin;
1	John Doe	5555	email@email.com
2	Jane Doe	5555	jane@email.com
\.


--
-- Data for Name: ProjectTags; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."ProjectTags" ("projectId", "tagName") FROM stdin;
1	ibm
2	apple
\.


--
-- Data for Name: Projects; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Projects" (id, description, at, due) FROM stdin;
1	Sell IBM system to Customer	2019-02-04 20:31:47.372433+00	2019-05-01 20:31:23.751+00
2	Sell Apple computers	2019-02-04 20:32:02.956887+00	2019-05-01 20:31:23.751+00
\.


--
-- Data for Name: Tags; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Tags" (name) FROM stdin;
lead
apple
ibm
\.


--
-- Name: Activities_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public."Activities_id_seq"', 1, false);


--
-- Name: Comments_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public."Comments_id_seq"', 2, true);


--
-- Name: Contacts_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public."Contacts_id_seq"', 2, true);


--
-- Name: Projects_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public."Projects_id_seq"', 2, true);


--
-- Name: Activities Activities_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Activities"
    ADD CONSTRAINT "Activities_pkey" PRIMARY KEY (id);


--
-- Name: Comments Comments_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Comments"
    ADD CONSTRAINT "Comments_pkey" PRIMARY KEY (id);


--
-- Name: ContactAssociations ContactAssociations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."ContactAssociations"
    ADD CONSTRAINT "ContactAssociations_pkey" PRIMARY KEY ("fromContactId", "toContactId");


--
-- Name: ContactTags ContactTags_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."ContactTags"
    ADD CONSTRAINT "ContactTags_pkey" PRIMARY KEY ("contactId", "tagName");


--
-- Name: Contacts Contacts_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Contacts"
    ADD CONSTRAINT "Contacts_pkey" PRIMARY KEY (id);


--
-- Name: ProjectTags ProjectTags_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."ProjectTags"
    ADD CONSTRAINT "ProjectTags_pkey" PRIMARY KEY ("projectId", "tagName");


--
-- Name: Projects Projects_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Projects"
    ADD CONSTRAINT "Projects_pkey" PRIMARY KEY (id);


--
-- Name: Tags Tags_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Tags"
    ADD CONSTRAINT "Tags_pkey" PRIMARY KEY (name);


--
-- PostgreSQL database dump complete
--

