import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';
import { useColorMode } from '@docusaurus/theme-common';

import HomepageFeatures from '../components/HomepageFeatures';


function HomepageHeader() {
    const { siteConfig } = useDocusaurusContext();
    const { isDarkTheme } = useColorMode();

    return (
        <>
            <header className='hero hero--primary text--center'>
                <div className="container">
                    <Heading as="h1" className="hero__title">
                        {siteConfig.title}
                    </Heading>
                    <p>{siteConfig.tagline}</p>
                    <Link
                        className="button button--secondary button--lg"
                        to="/docs/overview">
                        Start here 🚀
                    </Link>
                </div>
            </header>
        </>
    );
}

export default function Home() {
    const { siteConfig } = useDocusaurusContext();
    return (
        <Layout
            title={`${siteConfig.title}`}
            description="Description will go into a meta tag in <head />">
            <HomepageHeader />
            <main>
                <HomepageFeatures />
            </main>
        </Layout>
    );
}
